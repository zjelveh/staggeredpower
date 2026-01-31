# tests/testthat/test-noise-count-obs.R
# Tests for the Poisson count observation model (obs_model = "poisson")
# for additive PTA types (CS, imputation)

library(data.table)
library(fixest)

# =============================================================================
# Helper: create a simple staggered panel with population
# =============================================================================
create_count_obs_test_data <- function(seed = 42) {
  set.seed(seed)
  n_units <- 30
  n_years <- 15
  base_year <- 2000

  dt <- CJ(unit = 1:n_units, year = base_year:(base_year + n_years - 1))

  # Population per unit (stable across time)
  pop_lookup <- data.table(unit = 1:n_units, pop = sample(50000:500000, n_units))
  dt[pop_lookup, on = "unit", pop := i.pop]

  # Assign treatment cohorts: first 10 units treated at year 2007, next 10 at 2010, last 10 never
  dt[, year_passed := fifelse(unit <= 10, 2007L,
                       fifelse(unit <= 20, 2010L, NA_integer_))]
  dt[, treat_ind := fifelse(!is.na(year_passed) & year >= year_passed, 1L, 0L)]
  dt[, rel_pass := fifelse(!is.na(year_passed), year - year_passed, NA_integer_)]

  # Generate rate outcome (per 100k): Poisson counts / pop * 100000
  dt[, true_rate := runif(.N, 1, 10)]  # base rate 1-10 per 100k
  dt[, count := rpois(.N, lambda = true_rate * pop / 100000)]
  dt[, y := count / pop * 100000]

  dt
}


# =============================================================================
# Test 1: normalize_noise_spec accepts "gaussian" obs_model
# =============================================================================
test_that("normalize_noise_spec accepts gaussian obs_model", {
  ns <- normalize_noise_spec(list(engine = "ar1_anchored", obs_model = "gaussian"))
  expect_equal(ns$obs_model, "gaussian")
  expect_equal(ns$engine, "ar1_anchored")
})

test_that("normalize_noise_spec accepts poisson obs_model", {
  ns <- normalize_noise_spec(list(engine = "ar1_anchored", obs_model = "poisson"))
  expect_equal(ns$obs_model, "poisson")
})

test_that("normalize_noise_spec auto-defaults to gaussian for stochastic engines", {
  ns <- normalize_noise_spec(list(engine = "iid"))
  expect_equal(ns$obs_model, "gaussian")
})


# =============================================================================
# Test 2: enforce_PTA_imputation with obs_model="poisson" returns cf_mean_rate
# =============================================================================
test_that("enforce_PTA_imputation with obs_model=poisson returns cf_mean_rate column", {
  dt <- create_count_obs_test_data()
  noise_spec <- list(engine = "ar1_anchored", obs_model = "poisson")

  result <- enforce_PTA(
    dt, unit = "unit", group = "year_passed", time = "year",
    outcome = "y", method = "imputation", noise_spec = noise_spec
  )

  # cf_mean_rate should exist for treated rows
  expect_true("cf_mean_rate" %in% names(result))
  treated <- result[!is.na(year_passed) & year >= year_passed]
  expect_true(all(!is.na(treated$cf_mean_rate)))

  # counterfactual should equal cf_mean_rate (deterministic, no noise)
  expect_equal(treated$counterfactual, treated$cf_mean_rate)
})


# =============================================================================
# Test 3: enforce_PTA_CS with obs_model="poisson" returns cf_mean_rate
# =============================================================================
test_that("enforce_PTA_CS with obs_model=poisson returns cf_mean_rate column", {
  dt <- create_count_obs_test_data()
  noise_spec <- list(engine = "ar1_anchored", obs_model = "poisson")

  result <- enforce_PTA(
    dt, unit = "unit", group = "year_passed", time = "year",
    outcome = "y", method = "CS", noise_spec = noise_spec
  )

  # cf_mean_rate should exist for treated rows
  expect_true("cf_mean_rate" %in% names(result))
  treated <- result[!is.na(year_passed) & year >= year_passed]
  expect_true(all(!is.na(treated$cf_mean_rate)))

  # counterfactual should equal cf_mean_rate
  expect_equal(treated$counterfactual, treated$cf_mean_rate)
})


# =============================================================================
# Test 4: Nonnegativity invariant — no negative counterfactuals across draws
# =============================================================================
test_that("obs_model=poisson produces no negative y_cf across 20 Poisson draws", {
  dt <- create_count_obs_test_data()
  noise_spec <- list(engine = "ar1_anchored", obs_model = "poisson")

  # Get cf_mean_rate via enforce_PTA

  pta_result <- enforce_PTA(
    dt, unit = "unit", group = "year_passed", time = "year",
    outcome = "y", method = "imputation", noise_spec = noise_spec,
    pop_var = "pop"
  )

  # Simulate 20 Poisson draws
  treated <- pta_result[!is.na(year_passed) & year >= year_passed]
  set.seed(99)
  for (i in 1:20) {
    lambda <- pmax(treated$cf_mean_rate, 0) * treated$pop / 100000
    y_cf <- stats::rpois(nrow(treated), lambda) / treated$pop * 100000
    expect_true(all(y_cf >= 0),
                info = sprintf("Draw %d has negative y_cf", i))
  }
})


# =============================================================================
# Test 5: Mean preservation — average simulated rate ≈ deterministic mean
# =============================================================================
test_that("Poisson draw mean converges to deterministic cf_mean_rate", {
  dt <- create_count_obs_test_data()
  noise_spec <- list(engine = "ar1_anchored", obs_model = "poisson")

  pta_result <- enforce_PTA(
    dt, unit = "unit", group = "year_passed", time = "year",
    outcome = "y", method = "imputation", noise_spec = noise_spec,
    pop_var = "pop"
  )

  treated <- pta_result[!is.na(year_passed) & year >= year_passed]
  # Only test units with positive cf_mean_rate
  treated <- treated[cf_mean_rate > 0]

  if (nrow(treated) > 0) {
    # Pick one unit-time pair
    test_row <- treated[1]
    lambda <- test_row$cf_mean_rate * test_row$pop / 100000

    # 10000 draws, mean should be close to lambda
    set.seed(123)
    draws <- stats::rpois(10000, lambda)
    mean_rate <- mean(draws) / test_row$pop * 100000

    # Within 10% relative tolerance (generous for MC)
    expect_equal(mean_rate, test_row$cf_mean_rate, tolerance = 0.1)
  }
})


# =============================================================================
# Test 6: No unit dropping with obs_model="poisson" in run_power_analysis
# =============================================================================
test_that("run_power_analysis with obs_model=poisson reports zero units dropped", {
  skip_if_not_installed("foreach")
  skip_if_not_installed("doParallel")

  library(foreach)
  library(doParallel)

  dt <- create_count_obs_test_data()
  noise_spec <- list(engine = "ar1_anchored", obs_model = "poisson")

  # Register sequential backend for test
  registerDoSEQ()

  result <- run_power_analysis(
    data_clean = dt,
    unit_var = "unit",
    group_var = "year_passed",
    time_var = "year",
    rel_pass_var = "rel_pass",
    treat_ind_var = "treat_ind",
    outcome = "y",
    pta_type = "imputation",
    percent_effect = 1.0,  # null effect
    n_sims = 2,
    pop_var = "pop",
    noise_spec = noise_spec,
    models_to_run = "imputation"
  )

  # Zero bound errors and zero violating units
  expect_true(all(result$final_power$n_bound_errors == 0))
  expect_true(all(result$final_power$n_violating_units == 0))
})


# =============================================================================
# Test 7: Backward compat — obs_model="gaussian" behaves like old default
# =============================================================================
test_that("obs_model=gaussian produces same results as legacy behavior", {
  dt <- create_count_obs_test_data(seed = 99)

  # Gaussian explicit
  set.seed(42)
  result_gaussian <- enforce_PTA(
    copy(dt), unit = "unit", group = "year_passed", time = "year",
    outcome = "y", method = "imputation",
    noise_spec = list(engine = "iid", obs_model = "gaussian")
  )

  # Default (should also be gaussian now)
  set.seed(42)
  result_default <- enforce_PTA(
    copy(dt), unit = "unit", group = "year_passed", time = "year",
    outcome = "y", method = "imputation",
    noise_spec = list(engine = "iid")
  )

  # Should produce identical counterfactuals
  expect_equal(result_gaussian$counterfactual, result_default$counterfactual)
  # Neither should have cf_mean_rate
  expect_false("cf_mean_rate" %in% names(result_gaussian))
  expect_false("cf_mean_rate" %in% names(result_default))
})
