# R/calibrate_noise.R
# ---------------------------------------------------------------------------
# Data-driven calibration of the latent-noise spec (latent_sigma, latent_rho)
# for the power-simulation DGP. Nothing is tuned to a size/rejection target;
# the values come from the outcome's own dispersion structure.
# ---------------------------------------------------------------------------

#' Calibrate the latent-noise spec (sigma, rho) from data
#'
#' Derives the multiplicative latent-shock log-SD (\code{latent_sigma}) and its
#' AR(1) serial correlation (\code{latent_rho}) from the observed panel, so the
#' power DGP reproduces the outcome's real dispersion instead of hand-set values.
#'
#' \strong{Target.} \code{latent_sigma} is solved so the DGP's \emph{simulated}
#' per-unit residual SD -- measured off the same TWFE fit the estimators use, so
#' it accounts for re-fitting on noisy data -- matches the \emph{real} per-unit
#' residual SD. This is a like-for-like match to the cluster-level dispersion the
#' estimators' cluster-robust SEs depend on. \code{latent_rho} is the pooled
#' within-unit AR(1) coefficient of the residuals. A cheaper closed-form
#' \code{sigma_analytic} (moment match, no re-fit) is returned as the low end of a
#' sensitivity band -- report \code{sigma_range}, do not treat one value as exact.
#'
#' @param data Panel data (data.table or data.frame).
#' @param outcome,unit_var,time_var,treat_ind_var Column names.
#' @param family "rate" (Poisson-lognormal, returns \code{latent_sigma}) or
#'   "share" (Beta-Binomial, returns \code{latent_phi} -- the concentration).
#' @param pop_var Population column (required for family="rate").
#' @param total_count_var Binomial denominator column (required for family="share").
#' @param rate_scale Rate denominator scale (default 1e5).
#' @param n_cal_sims Sims per grid point in the sigma solve (default 60).
#' @param seed RNG seed for the calibration sim (default 1).
#' @param min_periods Minimum untreated periods per unit to enter the SD target (default 5).
#' @return A list. For \code{family="rate"}: \code{latent_sigma},
#'   \code{sigma_analytic}, \code{sigma_range}. For \code{family="share"}:
#'   \code{latent_phi} (Beta-Binomial concentration), \code{phi_analytic},
#'   \code{phi_range}. Both: \code{latent_rho}, \code{target_per_unit_sd}, \code{family}.
#' @export
calibrate_noise <- function(data, outcome, unit_var, time_var, treat_ind_var,
                            family = c("rate", "share"),
                            pop_var = NULL, total_count_var = NULL,
                            rate_scale = 1e5, n_cal_sims = 60L,
                            seed = 1L, min_periods = 5L,
                            group_var = NULL, cohort_trend = FALSE) {
  family <- match.arg(family)
  if (family == "rate"  && is.null(pop_var))         stop("family='rate' requires pop_var")
  if (family == "share" && is.null(total_count_var)) stop("family='share' requires total_count_var")
  expo_var <- if (family == "rate") pop_var else total_count_var

  d <- data.table::as.data.table(data)
  unt <- d[[treat_ind_var]] != 1
  # Restrict to units/years present among the untreated cells so the additive FE
  # prediction is defined for every remaining cell. Only-treated units (no untreated
  # coverage -- e.g. NIBRS states whose reporting starts after adoption) carry no
  # information about the untreated dispersion and would break predict() on unseen levels.
  .su <- unique(d[unt][[unit_var]]); .st <- unique(d[unt][[time_var]])
  d <- d[get(unit_var) %in% .su & get(time_var) %in% .st]
  unt <- d[[treat_ind_var]] != 1
  if (!requireNamespace("fixest", quietly = TRUE)) stop("calibrate_noise requires the 'fixest' package.")
  # DGP-consistent detrending. For the cs_cohort DGP (cohort_trend=TRUE) residualize
  # off the SAME non-additive surface the DGP uses as its mean -- state + year FE +
  # cohort-specific linear trend -- so the calibrated sigma/phi capture only the
  # idiosyncratic noise BEYOND it, not the cohort-trend variation the mean already
  # carries (avoiding the double-count). Plain two-way FE otherwise (the additive /
  # imputation DGP), where that variation genuinely IS noise.
  if (isTRUE(cohort_trend)) {
    if (is.null(group_var)) stop("calibrate_noise: cohort_trend=TRUE requires group_var")
    .gv <- d[[group_var]]
    d[, .cohf := factor(ifelse(is.na(.gv), 0, as.numeric(.gv)))]
    d[, .yrc  := as.numeric(get(time_var)) - mean(sort(unique(as.numeric(get(time_var)))))]
    if (!("0" %in% levels(d[[".cohf"]])))
      stop("calibrate_noise: cohort_trend needs never-treated (cohort 0) units for the reference")
  }
  resid_rhs <- if (isTRUE(cohort_trend)) "i(.cohf, .yrc, ref = '0')" else "1"
  fml <- stats::as.formula(sprintf("`%s` ~ %s | `%s` + `%s`", outcome, resid_rhs, unit_var, time_var))
  fit <- fixest::feols(fml, data = d[unt], notes = FALSE)
  mu <- as.numeric(stats::predict(fit, newdata = d))
  mu <- if (family == "share") pmin(pmax(mu, 1e-4), 1 - 1e-4) else pmax(mu, 0)
  d[, `:=`(.mu = mu, .expo = as.numeric(get(expo_var)), .u = get(unit_var), .t = get(time_var))]

  # --- real per-unit residual SD (the target) + per-unit A/B moments + rho ---
  un <- d[unt, .(.u, .t, .mu, .expo, r = get(outcome) - .mu)]
  un <- un[is.finite(r) & is.finite(.mu) & is.finite(.expo) & .expo > 0]   # drop feols-singleton NAs
  # A = per-cell sampling variance (Poisson rate / Binomial share).
  # B = overdispersion loading: rate -> mu^2 (multiplicative lognormal, term k = e^{sig^2}-1);
  #     share -> mu(1-mu)(1-1/n) (Beta-Binomial, term rho_icc = 1/(phi+1)).
  hs <- un[, .(sd = stats::sd(r), n = .N,
               A = if (family == "rate") mean(.mu * rate_scale / .expo) else mean(.mu * (1 - .mu) / .expo),
               B = if (family == "rate") mean(.mu^2) else mean(.mu * (1 - .mu) * pmax(1 - 1 / .expo, 0))),
           by = .u][n >= min_periods & sd > 0]
  target <- mean(hs$sd)
  data.table::setorder(un, .u, .t); un[, rl := data.table::shift(r), by = .u]
  rho <- tryCatch(as.numeric(stats::coef(stats::lm(r ~ 0 + rl, data = un[is.finite(rl)]))[1]),
                  error = function(e) 0)
  rho <- max(min(rho, 0.95), 0)
  base <- d[, .(.u, .t, .mu, .expo, treat = get(treat_ind_var))]
  if (isTRUE(cohort_trend)) base[, `:=`(.cohf = d$.cohf, .yrc = d$.yrc)]   # carry the detrend regressors into the sim re-fit

  if (family == "share") {
    # ------- Beta-Binomial concentration phi (bounded, mean-preserving share DGP) -------
    # Per-cell share variance = A + B*rho_icc with rho_icc = 1/(phi+1) in (0,1). Solve the
    # moment match for rho_icc, then phi; refine phi so the SIMULATED per-unit residual SD
    # (Beta-Binomial draw, Gaussian-copula AR(1)) matches the target -- mirrors the obs draw.
    rho_icc_an <- tryCatch(
      stats::uniroot(function(ri) mean(sqrt(pmax(hs$A + hs$B * ri, 0))) - target, c(1e-6, 1 - 1e-6))$root,
      error = function(e) if (target > mean(sqrt(hs$A + hs$B))) 1 - 1e-6 else 1e-3)
    phi_analytic <- 1 / rho_icc_an - 1
    sim_sd_bb <- function(phi, s0) {
      set.seed(s0)
      mean(vapply(seq_len(n_cal_sims), function(b) {
        bb <- data.table::copy(base); data.table::setorder(bb, .u, .t)
        bb[, z := {   # standardized AR(1) latent, used as the copula pnorm(z)
          n <- .N; v <- numeric(n); v[1] <- stats::rnorm(1, 0, 1)
          if (n > 1) { e <- stats::rnorm(n, 0, sqrt(1 - rho^2)); for (j in 2:n) v[j] <- rho * v[j - 1] + e[j] }
          v
        }, by = .u]
        bb[, ys := stats::rbinom(.N, size = pmax(round(.expo), 1L),
              prob = stats::qbeta(stats::pnorm(z), pmax(.mu, 1e-9) * phi, pmax(1 - .mu, 1e-9) * phi)) /
              pmax(round(.expo), 1L)]
        f <- fixest::feols(stats::as.formula(sprintf("ys ~ %s | `.u` + `.t`", resid_rhs)), data = bb[treat != 1], notes = FALSE)
        rr <- bb[treat != 1]; rr[, rr := ys - as.numeric(stats::predict(f, newdata = rr))]
        hh <- rr[is.finite(rr), .(sd = stats::sd(rr), nn = .N), by = .u][nn >= min_periods & sd > 0]
        mean(hh$sd)
      }, numeric(1)))
    }
    grid <- sort(unique(pmax(phi_analytic * c(0.4, 0.7, 1.0, 1.5, 2.5), 1e-3)))
    sd_grid <- vapply(seq_along(grid), function(i) sim_sd_bb(grid[i], seed + i), numeric(1))
    # sd is monotone DECREASING in phi (more concentration -> less overdispersion)
    phi_sim <- if (target >= max(sd_grid) || target <= min(sd_grid)) {
      grid[which.min(abs(sd_grid - target))]          # target outside grid -> nearest point
    } else {
      as.numeric(stats::approx(x = sd_grid, y = grid, xout = target)$y)
    }
    return(list(latent_phi = as.numeric(phi_sim), latent_rho = rho,
                phi_analytic = as.numeric(phi_analytic),
                phi_range = sort(c(as.numeric(phi_analytic), as.numeric(phi_sim))),
                latent_sigma = NA_real_,
                target_per_unit_sd = target, family = family))
  }

  # ------- family == "rate": multiplicative lognormal log-SD sigma -------
  # closed-form sigma: mean_u sqrt(A_u + B_u*k) = target, k = e^{sigma^2}-1
  k_an <- tryCatch(stats::uniroot(function(k) mean(sqrt(pmax(hs$A + hs$B * k, 0))) - target,
                                  c(0, 50))$root, error = function(e) 0)
  sigma_analytic <- sqrt(log1p(k_an))

  # sim-matched sigma: draw the DGP at a grid, re-fit TWFE, match per-unit SD
  sim_sd <- function(sig, s0) {
    set.seed(s0)
    mean(vapply(seq_len(n_cal_sims), function(b) {
      bb <- data.table::copy(base); data.table::setorder(bb, .u, .t)
      bb[, eta := {
        n <- .N; v <- numeric(n); v[1] <- stats::rnorm(1, 0, sig)
        if (n > 1) { e <- stats::rnorm(n, 0, sig * sqrt(1 - rho^2)); for (j in 2:n) v[j] <- rho * v[j - 1] + e[j] }
        v
      }, by = .u]
      bb[, ys := stats::rpois(.N, pmax(pmax(.mu, 0) * .expo / rate_scale * exp(eta - sig^2 / 2), 1e-8)) * rate_scale / .expo]
      f <- fixest::feols(stats::as.formula(sprintf("ys ~ %s | `.u` + `.t`", resid_rhs)), data = bb[treat != 1], notes = FALSE)
      rr <- bb[treat != 1]; rr[, rr := ys - as.numeric(stats::predict(f, newdata = rr))]  # feols drops singletons -> NA
      hh <- rr[is.finite(rr), .(sd = stats::sd(rr), nn = .N), by = .u][nn >= min_periods & sd > 0]
      mean(hh$sd)
    }, numeric(1)))
  }
  grid <- sort(unique(pmax(sigma_analytic * c(0.6, 0.85, 1.1, 1.4, 1.9), 1e-3)))
  sd_grid <- vapply(seq_along(grid), function(i) sim_sd(grid[i], seed + i), numeric(1))
  sigma_sim <- if (target <= min(sd_grid) || target >= max(sd_grid)) {
    grid[which.min(abs(sd_grid - target))]          # target outside grid -> nearest point
  } else {
    as.numeric(stats::approx(x = sd_grid, y = grid, xout = target)$y)
  }

  list(latent_sigma = as.numeric(sigma_sim), latent_rho = rho,
       sigma_analytic = as.numeric(sigma_analytic),
       sigma_range = sort(c(as.numeric(sigma_analytic), as.numeric(sigma_sim))),
       target_per_unit_sd = target, family = family)
}
