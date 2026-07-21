# R/control_mean.R
# ---------------------------------------------------------------------------
# Modular control-mean surfaces for the re-roll-controls power DGP.
#
# When noise_spec$reroll_controls is TRUE, the power simulation regenerates
# outcomes for BOTH treated and control units (see the obs block in
# run_power_analysis()). Treated units keep the harmonized .cs_delta_control
# counterfactual (cf_mean_rate); control units need their own mean surface,
# supplied by one of these pluggable functions.
#
# INTERFACE (every surface shares this exact signature):
#   fn(df, unit, time, outcome, treat_ind, pop_var = NULL) -> numeric vector,
#      one fitted MEAN (outcome / rate scale) per row of df. Only the CONTROL
#      rows' values are used downstream; treated rows keep cf_mean_rate.
#      Surfaces must be parallel-trends-consistent.
#
# TO ADD A SURFACE (next iterations): write cm_<name>() with the signature
# above and add it to .CONTROL_MEAN_SURFACES. Nothing else changes. Select at
# run time via noise_spec$control_mean = "<name>". Planned surfaces:
#   cm_did      - derived from did's own control-side (tighter CS match)
#   cm_block    - state-block residual resampling (diagnostic-3's DGP "C")
#   cm_meandep  - state-specific / mean-dependent dispersion
# ---------------------------------------------------------------------------

#' Two-way (unit + year) fitted surface from untreated cells
#'
#' \eqn{S(i,t)=\widehat\alpha_i+\widehat\gamma_t}, fit on untreated observations
#' with \code{fixest::feols} -- the SAME routine \code{calibrate_noise} and the
#' imputation estimator use, so the reroll surface and the estimator's own
#' counterfactual are the identical fit (they agree exactly wherever the design
#' is full rank; they differ only at unbalanced-panel edge cells). \code{feols}
#' returns NA for a cell whose unit or year level is absent from the untreated
#' fit; those fall back to the observed outcome (for control cells the observed
#' value IS the no-law mean).
#' @keywords internal
.twfe_fitted_untreated <- function(d, unit, time, outcome, untreated) {
  fml <- stats::as.formula(sprintf("`%s` ~ 1 | `%s` + `%s`", outcome, unit, time))
  fit <- fixest::feols(fml, data = d[untreated], notes = FALSE)
  S <- as.numeric(stats::predict(fit, newdata = d))
  bad <- !is.finite(S)
  if (any(bad)) S[bad] <- as.numeric(d[[outcome]])[bad]
  S
}

#' TWFE control-mean surface (default)
#'
#' Additive unit + year fixed effects fit on untreated observations only,
#' predicted for every row. Consistent with the additive parallel-trends both
#' the CS and imputation estimators assume.
#' @keywords internal
cm_twfe <- function(df, unit, time, outcome, treat_ind, pop_var = NULL) {
  d <- data.table::as.data.table(df)
  untreated <- d[[treat_ind]] != 1
  .twfe_fitted_untreated(d, unit, time, outcome, untreated)
}

#' CS-anchored control-mean surface (for the CS DGP)
#'
#' Anchors each eventually-treated unit's untreated mean to its REALIZED
#' pre-adoption value plus the common time trend,
#' \eqn{\mu(i,t) = Y_i(g-1) + (\gamma_t - \gamma_{g-1})}, matching the anchor
#' \code{enforce_PTA_CS} builds the treated counterfactual on. \code{cm_twfe}
#' instead centers the pre-period on the unit's AVERAGE untreated level
#' (\eqn{\alpha_i + \gamma_{g-1}}); the gap is the anchor residual
#' \eqn{r_i = Y_i(g-1) - (\alpha_i + \gamma_{g-1})}, a constant per-unit shift
#' that cancels in every change (so control roles / parallel trends are
#' preserved) but fixes the treated unit's anchor so CS is unbiased. The residual
#' is what distinguishes CS from imputation, so this keeps the CS DGP distinct.
#' Never-treated units have no anchor and fall back to \eqn{\alpha_i + \gamma_t}.
#' @keywords internal
cm_cs_anchor <- function(df, unit, time, outcome, treat_ind, pop_var = NULL) {
  d <- data.table::as.data.table(df)
  u <- as.character(d[[unit]]); tm <- d[[time]]; y <- d[[outcome]]; tr <- d[[treat_ind]]
  untreated <- tr != 1
  S <- .twfe_fitted_untreated(d, unit, time, outcome, untreated)   # S(i,t) = alpha_i + gamma_t (feols)
  # cohort g_i = first treated year per unit (eventually-treated); NA for never-treated
  g_by_unit <- tapply(tm[tr == 1], u[tr == 1], min)
  g <- as.numeric(g_by_unit[u])                                    # per-row cohort (NA = never-treated)
  # anchor Y_i(g-1) and S_i(g-1) from the pre-adoption row of each treated unit
  is_anchor <- !is.na(g) & tm == (g - 1)
  anchor_y <- tapply(y[is_anchor], u[is_anchor], function(z) z[1])
  anchor_S <- tapply(S[is_anchor], u[is_anchor], function(z) z[1])
  ay <- as.numeric(anchor_y[u]); aS <- as.numeric(anchor_S[u])
  # mu(i,t) = Y_i(g-1) + (gamma_t - gamma_{g-1}) = ay + (S - aS); never-treated / no anchor -> S
  as.numeric(ifelse(!is.na(g) & !is.na(ay), ay + (S - aS), S))
}

#' CS group-consistent control-mean surface (default for the CS DGP)
#'
#' Moves every control unit along a SINGLE calendar-time control path \eqn{C(t)},
#' anchored to each treated unit's realized pre-adoption value \eqn{Y_i(g-1)}. The
#' path is recovered from did's OWN control-side: \code{enforce_PTA_CS} sets the
#' treated counterfactual to \eqn{Y_i(g-1) + dc(g,t)} (stored in
#' \code{cf_mean_rate}), so \eqn{dc(g,t) = } \code{cf_mean_rate} \eqn{- Y_i(g-1)}
#' is did's implied control-side change for each cohort-time. We fit one path by
#' least squares so \eqn{C(t) - C(g-1) \approx dc(g,t)} across all cohorts. Because
#' the treated counterfactual and the control cells then move by the same \eqn{dc},
#' CS's own comparison and the DGP agree and the CS null bias is small. Unlike a
#' per-unit trend the surface stays smooth (so injected noise is not double-counted),
#' and unlike the global FE trend (\code{cm_cs_anchor}) it follows did's comparison
#' side rather than the all-states average. One path cannot match every cohort's
#' \eqn{dc} exactly (comparison groups diverge), leaving a small irreducible
#' residual -- the honest cost of a single control surface for cohort-specific
#' comparisons. Falls back to anchors when \code{cf_mean_rate} is unavailable.
#' @keywords internal
cm_cs_group <- function(df, unit, time, outcome, treat_ind, pop_var = NULL) {
  d <- data.table::as.data.table(df)
  u <- as.character(d[[unit]]); tm <- as.numeric(d[[time]]); y <- as.numeric(d[[outcome]]); tr <- d[[treat_ind]]
  cfm <- if ("cf_mean_rate" %in% names(d)) as.numeric(d[["cf_mean_rate"]]) else rep(NA_real_, nrow(d))
  # cohort g_i and realized anchor Y_i(g-1)
  g_by_unit <- tapply(tm[tr == 1], u[tr == 1], min)
  g <- as.numeric(g_by_unit[u])
  is_anchor <- !is.na(g) & tm == (g - 1)
  anchor_y <- tapply(y[is_anchor], u[is_anchor], function(z) z[1])
  ay <- as.numeric(anchor_y[u]); at <- g - 1
  # never-treated: anchor to own untreated mean at its mean time
  du <- data.table::data.table(u = u[tr != 1], t = tm[tr != 1], y = y[tr != 1])
  nts <- du[, .(yb = mean(y), tb = round(mean(t))), by = u]
  nt <- is.na(g); mi <- match(u[nt], nts$u); ay[nt] <- nts$yb[mi]; at[nt] <- nts$tb[mi]
  # did's own control-side dc(g,t) = cf_mean_rate - anchor, on treated cells
  keep <- tr == 1 & is.finite(cfm) & is.finite(ay)
  dc <- data.table::data.table(g = g[keep], t = tm[keep], d = cfm[keep] - ay[keep])[is.finite(d)][
        , .(d = mean(d)), by = .(g, t)]
  if (nrow(dc) < 2L) return(as.numeric(ay))                       # no usable dc -> anchors only
  # single calendar-time control path C(t): least squares C(t) - C(g-1) ~= dc(g,t)
  lev <- sort(unique(c(dc$t, dc$g - 1)))
  M <- matrix(0, nrow(dc), length(lev)); it <- match(dc$t, lev); ib <- match(dc$g - 1, lev)
  for (k in seq_len(nrow(dc))) { M[k, it[k]] <- M[k, it[k]] + 1; M[k, ib[k]] <- M[k, ib[k]] - 1 }
  cf <- tryCatch({ cc <- qr.coef(qr(M), dc$d); cc[is.na(cc)] <- 0; cc },
                 error = function(e) rep(0, length(lev)))
  allt <- sort(unique(tm)); Cfull <- stats::approx(lev, cf, xout = allt, rule = 2)$y
  Ct <- Cfull[match(tm, allt)]; Cat <- Cfull[match(at, allt)]
  Ct[!is.finite(Ct)] <- 0; Cat[!is.finite(Cat)] <- 0
  as.numeric(ay + (Ct - Cat))
}

#' CS-cohort untreated mean surface (option B: non-additive, satisfies CSA-not-BJS)
#'
#' Fits a SMOOTHED, deliberately NON-ADDITIVE untreated mean surface
#' \deqn{S(i,t) = \alpha_i + \gamma_t + \sum_g 1\{cohort_i = g\}\,\beta_g\,(t-\bar t)}
#' on the UNTREATED cells only (never-treated units + eventually-treated units'
#' pre-adoption years), then predicts \eqn{S} for every row. \eqn{\alpha_i} is a
#' state fixed effect, \eqn{\gamma_t} one COMMON nonparametric calendar-year path
#' (never-treated is the reference cohort, so its trend is folded into
#' \eqn{\gamma_t}), and \eqn{\beta_g} is each adoption cohort's OWN linear (or
#' polynomial, \code{trend_order = 2}) trend deviation from that common path.
#'
#' The \eqn{\beta_g} are what make \eqn{S} non-additive: with all \eqn{\beta_g=0}
#' this collapses to \eqn{\alpha_i+\gamma_t} (imputation's additive two-way
#' structure). The cohort-trend divergence \eqn{\beta_g - \bar\beta_{pool}} drives
#' both (i) the imputation bias (BJS cannot represent \eqn{\beta_g}) and (ii) the
#' CS pre-period placebos -- which do not test CSA's post-period restriction and
#' are irrelevant to its validity. Only the realized outcome ENTERS the fit (as a
#' smoothed mean); no realized cell is copied into \eqn{S}, so injected noise is
#' not recycled. \code{enforce_PTA_CS} anchors the treated counterfactual on
#' \eqn{S(i,g-1)} + did's own control-side change of \eqn{S}, so the CS estimand
#' is ~0 by construction while imputation is misspecified.
#' @keywords internal
cs_cohort_surface <- function(df, unit, group, time, outcome, trend_order = 1L) {
  d <- data.table::as.data.table(df)
  tm <- as.numeric(d[[time]]); g <- d[[group]]
  cohort <- ifelse(is.na(g), 0, as.numeric(g))
  treat  <- as.integer(cohort > 0 & tm >= cohort)
  DT <- data.table::data.table(
    .y   = as.numeric(d[[outcome]]),
    .uid = factor(as.character(d[[unit]])),
    .tt  = factor(tm),
    .coh = factor(cohort),
    .yrc = tm - mean(sort(unique(tm))))
  DT[, .yrc2 := .yrc^2]
  if (!("0" %in% levels(DT$.coh))) {
    stop("cs_cohort_surface: no never-treated (cohort 0) reference units; ",
         "the CS-cohort surface needs a never-treated group.", call. = FALSE)
  }
  untr <- DT[treat == 0 & is.finite(.y)]
  rhs <- if (trend_order >= 2L) "i(.coh, .yrc, ref = '0') + i(.coh, .yrc2, ref = '0')" else
                                "i(.coh, .yrc, ref = '0')"
  fit <- fixest::feols(stats::as.formula(paste0(".y ~ ", rhs, " | .uid + .tt")),
                       data = untr, notes = FALSE, warn = FALSE)
  S <- as.numeric(stats::predict(fit, newdata = DT))
  bad <- !is.finite(S)
  if (any(bad)) {                                   # fall back to unit untreated mean
    um <- untr[, .(m = mean(.y)), by = .uid]
    S[bad] <- um$m[match(DT$.uid[bad], um$.uid)]
    S[!is.finite(S)] <- mean(untr$.y)
  }
  S
}

# Registry of available control-mean surfaces. ADD NEW SURFACES HERE.
# NOTE: "cs_cohort" is handled specially in enforce_PTA_CS + the reroll block
# (it supplies the FULL mean surface, treated cells included), so it is not a
# per-row registry surface and is intentionally absent here.
.CONTROL_MEAN_SURFACES <- list(
  twfe      = cm_twfe,
  cs_anchor = cm_cs_anchor,
  cs_group  = cm_cs_group
)

#' Look up a control-mean surface by name (default "twfe").
#' @keywords internal
get_control_mean_surface <- function(name = "twfe") {
  if (is.null(name)) name <- "twfe"
  fn <- .CONTROL_MEAN_SURFACES[[name]]
  if (is.null(fn)) {
    stop(sprintf("Unknown control_mean surface '%s'. Available: %s",
                 name, paste(names(.CONTROL_MEAN_SURFACES), collapse = ", ")))
  }
  fn
}

#' Rough default latent-shock log-SD (fallback only)
#'
#' Callers should pass a calibrated \code{noise_spec$latent_sigma} (log-scale SD
#' of the multiplicative latent shock) estimated from the untreated overdispersion
#' with the mean structure removed. This fallback is deliberately crude and errs
#' toward zero; it exists so the DGP does not crash when latent_sigma is omitted.
#' @keywords internal
latent_sigma_default <- function(df, outcome, pop_var, treat_ind) {
  0.0
}
