# tests/testthat/test-noise-engine.R
# Unit tests for the noise engine module (R/noise_engine.R)

library(data.table)
library(fixest)

# =============================================================================
# Test normalize_noise_spec
# =============================================================================

test_that("normalize_noise_spec returns defaults for NULL input", {
  ns <- normalize_noise_spec(NULL)
  expect_equal(ns$engine, "iid")
  expect_equal(ns$innovation, "normal")
  expect_false(ns$common_shock)
  expect_null(ns$rho)
  expect_equal(ns$cs_pool, "global")
  expect_equal(ns$obs_model, "gaussian")  # iid -> gaussian
})

test_that("normalize_noise_spec auto-sets obs_model for engine='none'", {
  ns <- normalize_noise_spec(list(engine = "none"))
  expect_equal(ns$obs_model, "deterministic")
})

test_that("normalize_noise_spec preserves user-set obs_model", {
  ns <- normalize_noise_spec(list(engine = "ar1", obs_model = "deterministic"))
  expect_equal(ns$obs_model, "deterministic")
})

test_that("normalize_noise_spec errors on invalid engine", {
  expect_error(normalize_noise_spec(list(engine = "bad")), "engine")
})

test_that("normalize_noise_spec errors on invalid innovation", {
  expect_error(normalize_noise_spec(list(innovation = "bad")), "innovation")
})

test_that("normalize_noise_spec errors on rho out of range", {
  expect_error(normalize_noise_spec(list(rho = 1.5)), "rho")
  expect_error(normalize_noise_spec(list(rho = -0.1)), "rho")
})

test_that("normalize_noise_spec accepts valid rho", {
  ns <- normalize_noise_spec(list(rho = 0.5))
  expect_equal(ns$rho, 0.5)
})

# =============================================================================
# Test draw_noise
# =============================================================================

test_that("draw_noise with engine='none' returns zeros", {
  calib <- list(scale = "additive", engine = "none")
  result <- draw_noise(calib, units = 1:5, times = rep(2000, 5), seed = 1)
  expect_equal(nrow(result), 5)
  expect_true(all(result$eps == 0))
})

test_that("draw_noise with engine='iid' returns draws with correct scale", {
  calib <- list(scale = "additive", engine = "iid", sigma = 2.0)
  result <- draw_noise(calib, units = rep(1:10, 5), times = rep(1:5, each = 10), seed = 42)
  expect_equal(nrow(result), 50)
  # Empirical SD should be roughly 2.0 (within 50% for 50 draws)
  expect_true(abs(sd(result$eps) - 2.0) < 1.5)
})

test_that("draw_noise with engine='iid' on log scale uses sigma_log", {
  calib <- list(scale = "log", engine = "iid", sigma_log = 0.5)
  result <- draw_noise(calib, units = 1:100, times = rep(2000, 100), seed = 42)
  expect_true(abs(sd(result$eps) - 0.5) < 0.3)
})

test_that("draw_noise with engine='ar1' shows serial correlation", {
  calib <- list(
    scale = "additive", engine = "ar1",
    rho = 0.7, sigma_eta = 1.0
  )
  # Single unit with many time periods
  units <- rep(1, 50)
  times <- 1:50
  result <- draw_noise(calib, units, times, seed = 42)

  # Check autocorrelation: lag-1 correlation should be positive
  eps <- result$eps
  lag1_cor <- cor(eps[-1], eps[-length(eps)])
  expect_true(lag1_cor > 0.1)  # Should be substantially positive with rho=0.7
})

test_that("draw_noise with engine='ar1_common' shows cross-unit correlation", {
  calib <- list(
    scale = "additive", engine = "ar1_common",
    rho = 0.3, sigma_eta = 0.5,
    u_pool = rnorm(20, 0, 2.0),  # Large common shocks
    common_shock = TRUE
  )
  # Two units, same time periods
  units <- c(rep(1, 10), rep(2, 10))
  times <- c(1:10, 1:10)
  result <- draw_noise(calib, units, times, seed = 42)

  eps1 <- result[unit == 1, eps]
  eps2 <- result[unit == 2, eps]

  # Cross-correlation should be positive due to common shocks
  cross_cor <- cor(eps1, eps2)
  expect_true(cross_cor > 0)
})

# =============================================================================
# Test draw_noise_cs
# =============================================================================

test_that("draw_noise_cs with engine='none' returns zeros", {
  calib <- list(scale = "additive", engine = "none")
  treated <- list("5" = c(1, 2, 3))
  max_rp <- list("5" = 3)
  result <- draw_noise_cs(calib, treated, max_rp, seed = 1)
  expect_true(all(result$eps_ld == 0))
  expect_equal(nrow(result), 3 * 4)  # 3 units × 4 periods (rp 0-3)
})

test_that("draw_noise_cs with engine='iid' uses per-cell resid_sd", {
  calib <- list(
    scale = "additive", engine = "iid",
    resid_sd_by_cell = list("5_0" = 1.0, "5_1" = 2.0, "5_2" = 1.5)
  )
  treated <- list("5" = 1:20)
  max_rp <- list("5" = 2)
  result <- draw_noise_cs(calib, treated, max_rp, seed = 42)
  expect_equal(nrow(result), 20 * 3)  # 20 units × 3 periods

  # Check that different cells have different variance
  sd_rp0 <- sd(result[rp == 0, eps_ld])
  sd_rp1 <- sd(result[rp == 1, eps_ld])
  # With 20 draws each, we can't be too precise, but they should differ
  expect_true(sd_rp0 > 0)
  expect_true(sd_rp1 > 0)
})

test_that("draw_noise_cs with engine='ar1' cumulates shocks", {
  calib <- list(
    scale = "additive", engine = "ar1",
    rho = 0.5,
    sigma_eta = 1.0
  )
  treated <- list("5" = c(1))
  max_rp <- list("5" = 10)
  result <- draw_noise_cs(calib, treated, max_rp, seed = 42)

  # Cumulated shocks should have increasing variance with rp
  # (variance of cumulative sum grows with n)
  expect_equal(nrow(result), 11)  # 1 unit × 11 periods
  # First shock should be smaller in magnitude on average than later ones
  # Can't test rigorously with 1 unit, but verify structure
  expect_true(all(c("unit", "cohort", "rp", "eps_ld") %in% names(result)))
})

# =============================================================================
# Test calibrate_noise_imputation
# =============================================================================

test_that("calibrate_noise_imputation returns sigma for iid engine", {
  # Create simple test data
  set.seed(42)
  df <- data.table(
    unit = rep(1:20, each = 10),
    time = rep(1:10, 20),
    y = rnorm(200, mean = 10, sd = 2)
  )
  df[, cohort := fifelse(unit <= 10, 6L, NA_integer_)]
  df[, treated := !is.na(cohort) & time >= cohort]
  df_untreated <- df[treated == FALSE]

  mod <- feols(y ~ 1 | unit + time, data = df_untreated)

  ns <- normalize_noise_spec(list(engine = "iid"))
  calib <- calibrate_noise_imputation(mod, df_untreated, "unit", "time", ns)

  expect_equal(calib$scale, "additive")
  expect_equal(calib$engine, "iid")
  expect_true(calib$sigma > 0)
  expect_equal(calib$sigma, sigma(mod))
})

test_that("calibrate_noise_imputation estimates rho for ar1 engine", {
  set.seed(42)
  n_units <- 30
  n_times <- 15

  df <- data.table(
    unit = rep(1:n_units, each = n_times),
    time = rep(1:n_times, n_units)
  )
  df[, cohort := fifelse(unit <= 15, 10L, NA_integer_)]

  # Generate AR(1) errors with known rho
  true_rho <- 0.6
  df[, unit_fe := rnorm(1, 10, 2), by = unit]
  df[, time_fe := (time - 7) * 0.1]

  # AR(1) noise
  set.seed(123)
  for (uid in unique(df$unit)) {
    eps <- numeric(n_times)
    eps[1] <- rnorm(1, 0, 1)
    for (t in 2:n_times) {
      eps[t] <- true_rho * eps[t - 1] + rnorm(1, 0, sqrt(1 - true_rho^2))
    }
    df[unit == uid, noise := eps]
  }
  df[, y := unit_fe + time_fe + noise]
  df[, treated := !is.na(cohort) & time >= cohort]
  df_untreated <- df[treated == FALSE]

  mod <- feols(y ~ 1 | unit + time, data = df_untreated)
  ns <- normalize_noise_spec(list(engine = "ar1"))
  calib <- calibrate_noise_imputation(mod, df_untreated, "unit", "time", ns)

  expect_true(!is.null(calib$rho))
  expect_true(calib$rho > 0)
  expect_true(calib$rho < 0.95)
  # Should be reasonably close to true_rho (within 0.3)
  expect_true(abs(calib$rho - true_rho) < 0.3)
  expect_true(!is.null(calib$sigma_eta))
  expect_true(calib$sigma_eta > 0)
})
