# tests/testthat/test-noise-anchored.R
# Tests for the ar1_anchored noise engine

library(data.table)
library(fixest)

# =============================================================================
# Test 1: normalize_noise_spec accepts ar1_anchored
# =============================================================================

test_that("normalize_noise_spec accepts ar1_anchored engine", {
  ns <- normalize_noise_spec(list(engine = "ar1_anchored"))
  expect_equal(ns$engine, "ar1_anchored")
  expect_equal(ns$obs_model, "poisson")  # stochastic -> poisson
})

# =============================================================================
# Test 2: calibrate_noise_imputation stores control_resid_by_year, not u_pool
# =============================================================================

test_that("calibrate_noise_imputation with ar1_anchored stores control_resid_by_year", {
  # Create simple panel data
  set.seed(123)
  n_units <- 20
  n_years <- 10
  dt <- CJ(unit = 1:n_units, year = 2000:(2000 + n_years - 1))
  dt[, y := rnorm(.N, mean = 10, sd = 2)]

  # Fit unit + time FE model
  mod <- feols(y ~ 1 | unit + year, data = dt)
  noise_spec <- normalize_noise_spec(list(engine = "ar1_anchored"))

  calib <- calibrate_noise_imputation(mod, dt, "unit", "year", noise_spec)

  expect_equal(calib$engine, "ar1_anchored")
  expect_equal(calib$scale, "additive")
  expect_true(!is.null(calib$control_resid_by_year))
  expect_true(is.list(calib$control_resid_by_year))
  expect_true(length(calib$control_resid_by_year) > 0)
  # Should have rho and innovation parameters

  expect_true(!is.null(calib$rho))
  # ar1_anchored should NOT trigger u_pool (only ar1_common does)
  # Note: ar1_anchored doesn't match `engine == "ar1_common"` condition
  expect_null(calib$u_pool)
})

# =============================================================================
# Test 3: calibrate_noise_poisson has no u_pool, has jensen_correction
# =============================================================================

test_that("calibrate_noise_poisson with ar1_anchored has jensen_correction, no u_pool", {
  # Create count data
  set.seed(456)
  n_units <- 20
  n_years <- 10
  dt <- CJ(unit = 1:n_units, year = 2000:(2000 + n_years - 1))
  dt[, pop := rpois(.N, 100000)]
  dt[, count := rpois(.N, lambda = pop * 0.001)]

  # Fit Poisson model
  mod_pois <- fepois(count ~ 1 | unit + year, data = dt, offset = ~log(pop))
  noise_spec <- normalize_noise_spec(list(engine = "ar1_anchored"))

  calib <- calibrate_noise_poisson(mod_pois, dt, "count", "pop", "unit", "year", noise_spec)

  expect_equal(calib$engine, "ar1_anchored")
  expect_equal(calib$scale, "log")
  expect_true(isTRUE(calib$jensen_correction))
  # ar1_anchored does NOT match `engine == "ar1_common"` for u_pool
  expect_null(calib$u_pool)
  # Should have AR(1) parameters
  expect_true(!is.null(calib$rho))
})

# =============================================================================
# Test 4: draw_noise anchored additive — ATT-weighted mean ≈ 0
# =============================================================================

test_that("draw_noise ar1_anchored additive: ATT-weighted mean eps ≈ 0 across draws", {
  # Build a calibration object with known control residuals
  set.seed(789)
  calib <- list(
    scale = "additive",
    engine = "ar1_anchored",
    sigma = 1.0,
    rho = 0.3,
    sigma_eta = 0.8,
    control_resid_by_year = list(
      "2000" = rnorm(50, 0, 1),
      "2001" = rnorm(50, 0, 1),
      "2002" = rnorm(50, 0, 1),
      "2003" = rnorm(50, 0, 1),
      "2004" = rnorm(50, 0, 1)
    )
  )

  # Simulate treated panel: 10 units × 5 years
  units <- rep(1:10, each = 5)
  times <- rep(2000:2004, 10)

  # Draw many times and check ATT-weighted mean
  n_draws <- 500
  att_means <- numeric(n_draws)
  for (d in seq_len(n_draws)) {
    result <- draw_noise(calib, units, times)
    # ATT-weighted mean: uniform over all treated obs
    att_means[d] <- mean(result$eps)
  }

  # The ATT-weighted centering should keep the mean near zero
  # With 500 draws, the grand mean of att_means should be close to 0
  expect_true(abs(mean(att_means)) < 0.15,
              label = sprintf("ATT-weighted mean across draws = %.4f (should be near 0)", mean(att_means)))
})

# =============================================================================
# Test 5: draw_noise anchored log scale — Jensen correction: mean(exp(eps)) ≈ 1
# =============================================================================

test_that("draw_noise ar1_anchored log scale: mean(exp(eps)) ≈ 1 (Jensen correction)", {
  set.seed(101)
  calib <- list(
    scale = "log",
    engine = "ar1_anchored",
    sigma_log = 0.3,
    rho = 0.4,
    sigma_eta = 0.25,
    jensen_correction = TRUE
    # No control_resid_by_year — log scale has no common component
  )

  units <- rep(1:15, each = 8)
  times <- rep(2000:2007, 15)

  n_draws <- 200
  mean_exp_eps <- numeric(n_draws)
  for (d in seq_len(n_draws)) {
    result <- draw_noise(calib, units, times)
    mean_exp_eps[d] <- mean(exp(result$eps))
  }

  # With Jensen correction, each draw should have mean(exp(eps)) ≈ 1
  # Check that the average across draws is close to 1
  grand_mean <- mean(mean_exp_eps)
  expect_true(abs(grand_mean - 1) < 0.05,
              label = sprintf("mean(exp(eps)) = %.4f (should be near 1)", grand_mean))
})

# =============================================================================
# Test 6: draw_noise_cs treats ar1_anchored identically to ar1_common
# =============================================================================

test_that("draw_noise_cs with ar1_anchored matches ar1_common (seed-matched)", {
  # Build calibration with common shock pool
  calib_common <- list(
    scale = "additive",
    engine = "ar1_common",
    rho = 0.3,
    sigma_eta = 1.0,
    resid_sd_by_cell = list("2005_0" = 1, "2005_1" = 1, "2005_2" = 1),
    u_pool = rnorm(100, 0, 0.5)
  )
  calib_anchored <- calib_common
  calib_anchored$engine <- "ar1_anchored"

  treated_units <- list("2005" = 1:5)
  max_rp <- list("2005" = 2)

  set.seed(42)
  result_common <- draw_noise_cs(calib_common, treated_units, max_rp)

  set.seed(42)
  result_anchored <- draw_noise_cs(calib_anchored, treated_units, max_rp)

  # Should be identical (same code path for CS)
  expect_equal(result_common$eps_ld, result_anchored$eps_ld)
  expect_equal(nrow(result_common), nrow(result_anchored))
})
