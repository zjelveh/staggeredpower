# tests/testthat/test-enforce-pta-noise.R
# Integration tests for noise engine in enforce_PTA functions

library(data.table)
library(fixest)

# =============================================================================
# Helper: create test panel data
# =============================================================================
create_noise_test_data <- function(n_units = 30, n_times = 10, seed = 42) {
  set.seed(seed)
  df <- data.table(
    unit = rep(1:n_units, each = n_times),
    time = rep(1:n_times, n_units)
  )
  # Staggered treatment: units 1-10 at time 6, 11-20 at time 8, 21-30 never
  df[, cohort := fifelse(unit <= 10, 6L,
                          fifelse(unit <= 20, 8L, NA_integer_))]
  df[, unit_fe := rnorm(1, mean = 10, sd = 2), by = unit]
  df[, time_fe := (time - 5) * 0.3]
  df[, y := unit_fe + time_fe + rnorm(.N, 0, 0.5)]
  df[, treated := as.integer(!is.na(cohort) & time >= cohort)]
  df[, rel_pass := time - cohort]
  df[, .(unit, time, cohort, y, treated, rel_pass)]
}

create_poisson_test_data <- function(n_units = 30, n_times = 10, seed = 42) {
  set.seed(seed)
  df <- data.table(
    unit = rep(1:n_units, each = n_times),
    time = rep(1:n_times, n_units)
  )
  df[, cohort := fifelse(unit <= 10, 6L,
                          fifelse(unit <= 20, 8L, NA_integer_))]
  df[, pop := sample(50000:200000, 1), by = unit]
  df[, unit_fe := rnorm(1, mean = -10, sd = 0.3), by = unit]
  df[, time_fe := (time - 5) * 0.02]
  df[, log_rate := unit_fe + time_fe]
  df[, lambda := exp(log_rate) * pop]
  df[, count := rpois(.N, lambda)]
  df[, rate := count / pop * 100000]
  df[, treated := as.integer(!is.na(cohort) & time >= cohort)]
  df[, rel_pass := time - cohort]
  df[, .(unit, time, cohort, count, rate, pop, treated, rel_pass)]
}

# =============================================================================
# Test 1: Backward compat - imputation with default noise_spec = NULL
# =============================================================================
test_that("enforce_PTA_imputation: NULL noise_spec matches legacy behavior", {
  df <- create_noise_test_data()

  # With noise_spec = NULL (should default to iid)
  result_new <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "y",
    method = "imputation", seed = 42, noise_spec = NULL
  )

  # With explicit iid spec
  result_iid <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "y",
    method = "imputation", seed = 42,
    noise_spec = list(engine = "iid", innovation = "normal")
  )

  # Should be identical (same seed, same engine)
  expect_equal(result_new$counterfactual, result_iid$counterfactual)
})

# =============================================================================
# Test 2: No-noise imputation returns exact mu_hat
# =============================================================================
test_that("enforce_PTA_imputation: engine='none' gives deterministic output", {
  df <- create_noise_test_data()

  result1 <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "y",
    method = "imputation", seed = 1,
    noise_spec = list(engine = "none")
  )
  result2 <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "y",
    method = "imputation", seed = 999,
    noise_spec = list(engine = "none")
  )

  # Different seeds should produce IDENTICAL results (no noise)
  expect_equal(result1$counterfactual, result2$counterfactual)
})

# =============================================================================
# Test 3: No-noise CS returns exact mu_hat
# =============================================================================
test_that("enforce_PTA_CS: engine='none' gives deterministic output", {
  df <- create_noise_test_data()

  result1 <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "y",
    method = "CS", seed = 1,
    noise_spec = list(engine = "none")
  )
  result2 <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "y",
    method = "CS", seed = 999,
    noise_spec = list(engine = "none")
  )

  expect_equal(result1$counterfactual, result2$counterfactual)
})

# =============================================================================
# Test 4: No-noise Poisson returns deterministic output
# =============================================================================
test_that("enforce_PTA_poisson: engine='none' gives deterministic output", {
  df <- create_poisson_test_data()

  result1 <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "rate",
    method = "poisson", seed = 1,
    pop_var = "pop", outcome_type = "rate",
    noise_spec = list(engine = "none")
  )
  result2 <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "rate",
    method = "poisson", seed = 999,
    pop_var = "pop", outcome_type = "rate",
    noise_spec = list(engine = "none")
  )

  # Should be identical (deterministic obs_model too)
  expect_equal(result1$counterfactual, result2$counterfactual)
  # Non-negative
  expect_true(all(result1$counterfactual >= 0))
})

# =============================================================================
# Test 5: AR(1) imputation produces valid output
# =============================================================================
test_that("enforce_PTA_imputation: engine='ar1' runs without error", {
  df <- create_noise_test_data()

  result <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "y",
    method = "imputation", seed = 42,
    noise_spec = list(engine = "ar1", innovation = "normal")
  )

  expect_true("counterfactual" %in% names(result))
  expect_false(any(is.na(result$counterfactual)))
  expect_equal(nrow(result), nrow(df))
})

# =============================================================================
# Test 6: AR(1) + common shocks imputation
# =============================================================================
test_that("enforce_PTA_imputation: engine='ar1_common' runs without error", {
  df <- create_noise_test_data()

  result <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "y",
    method = "imputation", seed = 42,
    noise_spec = list(engine = "ar1_common", innovation = "empirical")
  )

  expect_true("counterfactual" %in% names(result))
  expect_false(any(is.na(result$counterfactual)))
})

# =============================================================================
# Test 7: AR(1) CS runs without error
# =============================================================================
test_that("enforce_PTA_CS: engine='ar1' runs without error", {
  df <- create_noise_test_data()

  result <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "y",
    method = "CS", seed = 42,
    noise_spec = list(engine = "ar1", innovation = "normal")
  )

  expect_true("counterfactual" %in% names(result))
  expect_false(any(is.na(result$counterfactual)))
})

# =============================================================================
# Test 8: AR(1) + common shocks CS
# =============================================================================
test_that("enforce_PTA_CS: engine='ar1_common' runs without error", {
  df <- create_noise_test_data()

  result <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "y",
    method = "CS", seed = 42,
    noise_spec = list(engine = "ar1_common", innovation = "empirical")
  )

  expect_true("counterfactual" %in% names(result))
  expect_false(any(is.na(result$counterfactual)))
})

# =============================================================================
# Test 9: AR(1) Poisson runs without error
# =============================================================================
test_that("enforce_PTA_poisson: engine='ar1' runs without error", {
  df <- create_poisson_test_data()

  result <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "rate",
    method = "poisson", seed = 42,
    pop_var = "pop", outcome_type = "rate",
    noise_spec = list(engine = "ar1", innovation = "normal")
  )

  expect_true("counterfactual" %in% names(result))
  expect_false(any(is.na(result$counterfactual)))
  expect_true(all(result$counterfactual >= 0))
})

# =============================================================================
# Test 10: enforce_PTA has noise_spec parameter
# =============================================================================
test_that("enforce_PTA has noise_spec parameter", {
  params <- names(formals(enforce_PTA))
  expect_true("noise_spec" %in% params)
})

# =============================================================================
# Test 11: run_power_analysis has noise_spec parameter
# =============================================================================
test_that("run_power_analysis has noise_spec parameter", {
  params <- names(formals(run_power_analysis))
  expect_true("noise_spec" %in% params)
})

# =============================================================================
# Test 12: Empirical innovations work for imputation
# =============================================================================
test_that("enforce_PTA_imputation: empirical innovations work", {
  df <- create_noise_test_data()

  result <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "y",
    method = "imputation", seed = 42,
    noise_spec = list(engine = "ar1", innovation = "empirical")
  )

  expect_true("counterfactual" %in% names(result))
  expect_false(any(is.na(result$counterfactual)))
})

# =============================================================================
# Test 13: CS with cohort-specific pooling
# =============================================================================
test_that("enforce_PTA_CS: cs_pool='cohort' runs without error", {
  df <- create_noise_test_data()

  result <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "y",
    method = "CS", seed = 42,
    noise_spec = list(engine = "ar1", cs_pool = "cohort")
  )

  expect_true("counterfactual" %in% names(result))
  expect_false(any(is.na(result$counterfactual)))
})

# =============================================================================
# Test 14: Noise introduces variability (iid produces different results with different seeds)
# =============================================================================
test_that("enforce_PTA_imputation: iid engine varies across seeds", {
  df <- create_noise_test_data()

  result1 <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "y",
    method = "imputation", seed = 1,
    noise_spec = list(engine = "iid")
  )
  result2 <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "y",
    method = "imputation", seed = 2,
    noise_spec = list(engine = "iid")
  )

  # Treated observations should differ
  treated_mask <- !is.na(df$cohort) & df$time >= df$cohort
  expect_false(all(result1$counterfactual[treated_mask] == result2$counterfactual[treated_mask]))
})

# =============================================================================
# Test 15: No temp columns left behind
# =============================================================================
test_that("enforce_PTA methods clean up temp columns", {
  df <- create_noise_test_data()

  for (method in c("imputation", "CS")) {
    result <- enforce_PTA(
      df, unit = "unit", group = "cohort", time = "time", outcome = "y",
      method = method, seed = 42,
      noise_spec = list(engine = "ar1")
    )
    # No columns starting with "." should remain (temp columns)
    dot_cols <- grep("^\\.", names(result), value = TRUE)
    expect_equal(length(dot_cols), 0,
                 info = paste("Method", method, "left temp columns:", paste(dot_cols, collapse = ", ")))
  }
})
