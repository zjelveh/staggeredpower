# R/inference_wcr.R
# ---------------------------------------------------------------------------
# Restricted wild cluster bootstrap (WCR-t) inference for a staggered-DiD ATT.
# Few-cluster analytic cluster-robust SEs over-reject; this reconstructs the
# null distribution of the studentized statistic by sign-flipping cluster
# residual blocks, and returns the correct critical value / p-value. It is a
# general overlay on the estimator, not wired into the power grid.
# ---------------------------------------------------------------------------

#' Restricted wild cluster bootstrap test for a staggered-DiD ATT
#'
#' Tests H0: overall ATT = 0 using a restricted wild cluster bootstrap
#' (Cameron-Gelbach-Miller). The analytic cluster-robust SEs of staggered-DiD
#' estimators over-reject with few clusters (the norm for state-policy designs);
#' WCR-t builds the null distribution of \code{t = ATT / SE} empirically and
#' returns the correct critical value instead of trusting 1.96.
#'
#' \strong{Mechanism.} Fit the null model \code{outcome ~ unit + time} on the
#' UNTREATED cells (the estimator's own counterfactual model), predict every
#' cell to get \code{yhat}, and residuals \code{uhat = outcome - yhat}
#' (treated-post residuals carry the effect + noise). For each replication draw
#' one weight per CLUSTER, form \code{y* = yhat + w * uhat} (whole cluster block
#' flipped, preserving within-cluster correlation), re-run the estimator, and
#' studentize \code{t* = ATT* / SE*}. The 97.5th percentile of \code{|t*|} is the
#' critical value.
#'
#' @param data Panel data.
#' @param outcome,unit_var,time_var,group_var,treat_ind_var Column names. \code{group_var}
#'   is the cohort (0/NA = never-treated), as the estimators expect.
#' @param estimator "imputation" (didimputation) or "cs" (did::att_gt). CS is
#'   studentized by its analytic (influence-function) SE for speed.
#' @param cluster_var Cluster column (default \code{unit_var}).
#' @param B Bootstrap replications (default 999).
#' @param weights "rademacher" (2-point; default) or "webb" (6-point; smoother
#'   with very few clusters).
#' @param est_method,base_period,allow_unbalanced_panel Passed to CS.
#' @param seed Optional RNG seed.
#' @return list(estimate, se, t_obs, wcr_cval, reject, wcr_pval, B, B_finite,
#'   weights, estimator).
#' @export
wcr_test <- function(data, outcome, unit_var, time_var, group_var, treat_ind_var,
                     estimator = c("imputation", "cs"),
                     cluster_var = NULL, B = 999L,
                     weights = c("rademacher", "webb"),
                     est_method = "dr", base_period = "universal",
                     allow_unbalanced_panel = TRUE, seed = NULL) {
  estimator <- match.arg(estimator); weights <- match.arg(weights)
  if (!is.null(seed)) set.seed(seed)
  if (estimator == "imputation" && !requireNamespace("didimputation", quietly = TRUE))
    stop("wcr_test(estimator='imputation') requires the 'didimputation' package.")
  if (estimator == "cs" && !requireNamespace("did", quietly = TRUE))
    stop("wcr_test(estimator='cs') requires the 'did' package.")
  if (!requireNamespace("fixest", quietly = TRUE))
    stop("wcr_test requires the 'fixest' package for the restricted null fit.")
  d <- data.table::as.data.table(data)
  if (is.null(cluster_var)) cluster_var <- unit_var
  # never-treated coded as NA -> 0, matching the estimators' expectation
  if (any(is.na(d[[group_var]]))) d[is.na(get(group_var)), (group_var) := 0]

  # estimator -> c(estimate, analytic se) on outcome column `yname`
  fit_est <- function(dd, yname) {
    if (estimator == "imputation") {
      m <- tryCatch(didimputation::did_imputation(
             data = as.data.frame(dd), yname = yname, gname = group_var,
             tname = time_var, idname = unit_var, cluster_var = cluster_var),
             error = function(e) NULL)
      if (is.null(m)) return(c(NA_real_, NA_real_))
      r <- as.data.frame(m)[1, ]; c(r$estimate, r$std.error)
    } else {
      a <- tryCatch(did::att_gt(
             yname = yname, tname = time_var, idname = unit_var, gname = group_var,
             xformla = ~1, data = as.data.frame(dd), control_group = "notyettreated",
             allow_unbalanced_panel = allow_unbalanced_panel, est_method = est_method,
             bstrap = FALSE, base_period = base_period), error = function(e) NULL)
      if (is.null(a)) return(c(NA_real_, NA_real_))
      s <- tryCatch(did::aggte(a, type = "simple", na.rm = TRUE), error = function(e) NULL)
      if (is.null(s)) return(c(NA_real_, NA_real_))
      c(s$overall.att, s$overall.se)
    }
  }

  # observed studentized statistic
  o <- fit_est(d, outcome); t_obs <- o[1] / o[2]
  if (!is.finite(t_obs)) stop("wcr_test: observed estimate/SE is not finite.")

  # restricted null fit on UNTREATED cells; predict every cell
  unt <- d[[treat_ind_var]] != 1
  rf <- fixest::feols(stats::as.formula(sprintf("`%s` ~ 1 | `%s` + `%s`", outcome, unit_var, time_var)),
                      data = d[unt], notes = FALSE)
  yhat <- as.numeric(stats::predict(rf, newdata = d))
  uhat <- d[[outcome]] - yhat
  inert <- !is.finite(yhat) | !is.finite(uhat)   # year FE unidentified (no never-treated) -> keep observed

  draw_w <- function(G) {
    if (weights == "rademacher") sample(c(-1, 1), G, replace = TRUE)
    else sample(c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5)), G, replace = TRUE)
  }
  clusters <- unique(d[[cluster_var]]); ckey <- match(d[[cluster_var]], clusters)
  yobs <- d[[outcome]]; ycol <- ".y_wcr"

  tstar <- numeric(B)
  for (b in seq_len(B)) {
    w  <- draw_w(length(clusters))
    ys <- yhat + w[ckey] * uhat
    ys[inert] <- yobs[inert]
    d[, (ycol) := ys]
    ob <- fit_est(d, ycol); tstar[b] <- ob[1] / ob[2]
  }
  if (ycol %in% names(d)) d[, (ycol) := NULL]
  tstar <- tstar[is.finite(tstar)]
  # Two-sided level-0.05 test on the FOLDED statistic |t*|: reject when |t_obs|
  # exceeds the 0.95 quantile of |t*| (P(|t*|>cval)=0.05). Using 0.975 here would
  # implement a 2.5% test and disagree with the two-sided wcr_pval below.
  cval  <- as.numeric(stats::quantile(abs(tstar), 0.95))

  list(estimate = o[1], se = o[2], t_obs = t_obs,
       wcr_cval = cval, reject = as.integer(abs(t_obs) > cval),
       wcr_pval = mean(abs(tstar) >= abs(t_obs)),
       B = B, B_finite = length(tstar), weights = weights, estimator = estimator)
}
