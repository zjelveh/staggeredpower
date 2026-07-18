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
#' @param family "rate" (Poisson-lognormal) or "share" (logit-normal-Binomial).
#' @param pop_var Population column (required for family="rate").
#' @param total_count_var Binomial denominator column (required for family="share").
#' @param rate_scale Rate denominator scale (default 1e5).
#' @param n_cal_sims Sims per grid point in the sigma solve (default 60).
#' @param seed RNG seed for the calibration sim (default 1).
#' @param min_periods Minimum untreated periods per unit to enter the SD target (default 5).
#' @return A list with \code{latent_sigma}, \code{latent_rho}, \code{sigma_analytic},
#'   \code{sigma_range}, \code{target_per_unit_sd}, and \code{family}.
#' @export
calibrate_noise <- function(data, outcome, unit_var, time_var, treat_ind_var,
                            family = c("rate", "share"),
                            pop_var = NULL, total_count_var = NULL,
                            rate_scale = 1e5, n_cal_sims = 60L,
                            seed = 1L, min_periods = 5L) {
  family <- match.arg(family)
  if (family == "rate"  && is.null(pop_var))         stop("family='rate' requires pop_var")
  if (family == "share" && is.null(total_count_var)) stop("family='share' requires total_count_var")
  expo_var <- if (family == "rate") pop_var else total_count_var

  d <- data.table::as.data.table(data)
  unt <- d[[treat_ind_var]] != 1
  fml <- stats::as.formula(sprintf("`%s` ~ factor(`%s`) + factor(`%s`)", outcome, unit_var, time_var))
  fit <- stats::lm(fml, data = d[unt])
  mu <- as.numeric(stats::predict(fit, newdata = d))
  mu <- if (family == "share") pmin(pmax(mu, 1e-4), 1 - 1e-4) else pmax(mu, 0)
  d[, `:=`(.mu = mu, .expo = as.numeric(get(expo_var)), .u = get(unit_var), .t = get(time_var))]

  # --- real per-unit residual SD (the target) + per-unit A/B moments + rho ---
  un <- d[unt, .(.u, .t, .mu, .expo, r = get(outcome) - .mu)]
  hs <- un[, .(sd = stats::sd(r), n = .N,
               A = if (family == "rate") mean(.mu * rate_scale / .expo) else mean(.mu * (1 - .mu) / .expo),
               B = if (family == "rate") mean(.mu^2)                    else mean((.mu * (1 - .mu))^2)),
           by = .u][n >= min_periods & sd > 0]
  target <- mean(hs$sd)
  data.table::setorder(un, .u, .t); un[, rl := data.table::shift(r), by = .u]
  rho <- tryCatch(as.numeric(stats::coef(stats::lm(r ~ 0 + rl, data = un[is.finite(rl)]))[1]),
                  error = function(e) 0)
  rho <- max(min(rho, 0.95), 0)

  # --- closed-form (analytic) sigma: mean_u sqrt(A_u + B_u*k) = target ---
  #     k = e^{sigma^2}-1 (rate, lognormal) or sigma^2 (share, logit delta method)
  k_an <- tryCatch(stats::uniroot(function(k) mean(sqrt(pmax(hs$A + hs$B * k, 0))) - target,
                                  c(0, 50))$root, error = function(e) 0)
  sigma_analytic <- if (family == "rate") sqrt(log1p(k_an)) else sqrt(k_an)

  # --- sim-matched sigma: draw the DGP at a grid, re-fit TWFE, match per-unit SD ---
  base <- d[, .(.u, .t, .mu, .expo, treat = get(treat_ind_var))]
  sim_sd <- function(sig, s0) {
    set.seed(s0)
    mean(vapply(seq_len(n_cal_sims), function(b) {
      bb <- data.table::copy(base); data.table::setorder(bb, .u, .t)
      bb[, eta := {
        n <- .N; v <- numeric(n); v[1] <- stats::rnorm(1, 0, sig)
        if (n > 1) { e <- stats::rnorm(n, 0, sig * sqrt(1 - rho^2)); for (j in 2:n) v[j] <- rho * v[j - 1] + e[j] }
        v
      }, by = .u]
      if (family == "rate") {
        bb[, ys := stats::rpois(.N, pmax(pmax(.mu, 0) * .expo / rate_scale * exp(eta - sig^2 / 2), 1e-8)) * rate_scale / .expo]
      } else {
        bb[, ys := stats::rbinom(.N, size = pmax(round(.expo), 1L),
              prob = stats::plogis(stats::qlogis(.mu) + eta)) / pmax(round(.expo), 1L)]
      }
      f <- stats::lm(ys ~ factor(.u) + factor(.t), data = bb[treat != 1])
      rr <- bb[treat != 1]; rr[, rr := ys - as.numeric(stats::predict(f))]
      hh <- rr[, .(sd = stats::sd(rr), nn = .N), by = .u][nn >= min_periods & sd > 0]
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
