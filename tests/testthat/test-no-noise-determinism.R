# tests/testthat/test-no-noise-determinism.R
# Regression tests for engine="none" deterministic benchmark mode

library(data.table)
library(fixest)
library(foreach)
library(doParallel)

# Register a sequential backend so %dopar% works in tests
registerDoSEQ()

# =============================================================================
# Helper: create test panel data (same as test-enforce-pta-noise.R)
# =============================================================================
create_determinism_test_data <- function(n_units = 30, n_times = 10, seed = 42) {
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
  df[, treat_ind := as.integer(!is.na(cohort) & time >= cohort)]
  df[, rel_pass := time - cohort]
  df[, .(unit, time, cohort, y, treat_ind, rel_pass)]
}

create_determinism_poisson_data <- function(n_units = 30, n_times = 10, seed = 42) {
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
  df[, treat_ind := as.integer(!is.na(cohort) & time >= cohort)]
  df[, rel_pass := time - cohort]
  df[, .(unit, time, cohort, count, rate, pop, treat_ind, rel_pass)]
}


# =============================================================================
# Test 1: Seed invariance — engine="none" produces identical results with different seeds
# =============================================================================
test_that("engine='none' produces identical counterfactuals regardless of seed (imputation)", {
  df <- create_determinism_test_data()

  result_seed42 <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "y",
    method = "imputation", seed = 42,
    noise_spec = list(engine = "none")
  )

  result_seed999 <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "y",
    method = "imputation", seed = 999,
    noise_spec = list(engine = "none")
  )

  expect_identical(result_seed42$counterfactual, result_seed999$counterfactual)
})

test_that("engine='none' produces identical counterfactuals regardless of seed (CS)", {
  df <- create_determinism_test_data()

  result_seed42 <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "y",
    method = "CS", seed = 42,
    noise_spec = list(engine = "none")
  )

  result_seed999 <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "y",
    method = "CS", seed = 999,
    noise_spec = list(engine = "none")
  )

  expect_identical(result_seed42$counterfactual, result_seed999$counterfactual)
})

test_that("engine='none' produces identical counterfactuals regardless of seed (poisson)", {
  df <- create_determinism_poisson_data()

  result_seed42 <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "rate",
    method = "poisson", seed = 42, pop_var = "pop", outcome_type = "rate",
    noise_spec = list(engine = "none")
  )

  result_seed999 <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "rate",
    method = "poisson", seed = 999, pop_var = "pop", outcome_type = "rate",
    noise_spec = list(engine = "none")
  )

  expect_identical(result_seed42$counterfactual, result_seed999$counterfactual)
})


# =============================================================================
# Test 2: n_sims guard — engine="none" + n_sims > 1 emits message and sets n_sims=1
# =============================================================================
test_that("run_power_analysis emits message and sets n_sims=1 when engine='none'", {
  df <- create_determinism_test_data()

  # Should emit a message about setting n_sims=1
  expect_message(
    result <- run_power_analysis(
      data_clean = df,
      unit_var = "unit",
      group_var = "cohort",
      time_var = "time",
      rel_pass_var = "rel_pass",
      treat_ind_var = "treat_ind",
      outcome = "y",
      pta_type = "imputation",
      percent_effect = 0.10,
      n_sims = 50,
      noise_spec = list(engine = "none"),
      models_to_run = "imputation"
    ),
    "engine='none' produces deterministic data; setting n_sims=1"
  )

  # Should have exactly 1 sim in results (one per model)
  expect_equal(max(result$final_power$sim), 1L)
})


# =============================================================================
# Test 3: One-pass violation loop — no re-enforcement under engine="none"
# =============================================================================
test_that("engine='none' uses single-pass violation check (no while loop)", {
  # Create data where some units will have negative counterfactuals
  # (forcing a PTA violation) to test single-pass behavior
  set.seed(42)
  df <- data.table(
    unit = rep(1:20, each = 10),
    time = rep(1:10, 20)
  )
  df[, cohort := fifelse(unit <= 5, 6L,
                          fifelse(unit <= 10, 8L, NA_integer_))]
  # Give some units very negative FE so counterfactuals go negative
  df[, unit_fe := fifelse(unit <= 3, -5, rnorm(1, mean = 10, sd = 1)), by = unit]
  df[, time_fe := (time - 5) * 0.3]
  df[, y := pmax(0, unit_fe + time_fe + rnorm(.N, 0, 0.5))]
  df[, treat_ind := as.integer(!is.na(cohort) & time >= cohort)]
  df[, rel_pass := time - cohort]

  # This should run without infinite loops and complete quickly
  # Under the old code with while loop + engine="none", this would loop forever
  # because rerunning produces identical violations
  result <- run_power_analysis(
    data_clean = df,
    unit_var = "unit",
    group_var = "cohort",
    time_var = "time",
    rel_pass_var = "rel_pass",
    treat_ind_var = "treat_ind",
    outcome = "y",
    pta_type = "imputation",
    percent_effect = 0.10,
    n_sims = 1,
    noise_spec = list(engine = "none"),
    models_to_run = "imputation"
  )

  # Should complete and return results

  expect_true(!is.null(result$final_power))
  expect_true(nrow(result$final_power) > 0)
})


# =============================================================================
# Test 4: iid backward compatibility — engine="iid" with fixed seed is reproducible
# =============================================================================
test_that("engine='iid' with fixed seed produces reproducible results (imputation)", {
  df <- create_determinism_test_data()

  result1 <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "y",
    method = "imputation", seed = 42,
    noise_spec = list(engine = "iid")
  )

  result2 <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "y",
    method = "imputation", seed = 42,
    noise_spec = list(engine = "iid")
  )

  expect_identical(result1$counterfactual, result2$counterfactual)
})

test_that("engine='iid' produces DIFFERENT results from engine='none' (imputation)", {
  df <- create_determinism_test_data()

  result_none <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "y",
    method = "imputation", seed = 42,
    noise_spec = list(engine = "none")
  )

  result_iid <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "y",
    method = "imputation", seed = 42,
    noise_spec = list(engine = "iid")
  )

  # The untreated observations should be identical (both keep observed values)
  # But treated observations should differ (none = mu_hat, iid = mu_hat + noise)
  # Check that at least some counterfactuals differ
  expect_false(identical(result_none$counterfactual, result_iid$counterfactual))
})


# =============================================================================
# Test 5: Output metadata — noise_engine, obs_model, design_resample in results
# =============================================================================
test_that("run_power_analysis includes noise metadata in output columns", {
  df <- create_determinism_test_data()

  result <- run_power_analysis(
    data_clean = df,
    unit_var = "unit",
    group_var = "cohort",
    time_var = "time",
    rel_pass_var = "rel_pass",
    treat_ind_var = "treat_ind",
    outcome = "y",
    pta_type = "imputation",
    percent_effect = 0.10,
    n_sims = 1,
    noise_spec = list(engine = "none"),
    models_to_run = "imputation"
  )

  # Check metadata columns exist
  expect_true("noise_engine" %in% names(result$final_power))
  expect_true("obs_model" %in% names(result$final_power))
  expect_true("design_resample" %in% names(result$final_power))

  # Check values
  expect_equal(unique(result$final_power$noise_engine), "none")
  expect_equal(unique(result$final_power$obs_model), "deterministic")
  expect_equal(unique(result$final_power$design_resample), "none")
})

test_that("run_power_analysis metadata reflects iid engine when specified", {
  df <- create_determinism_test_data()

  result <- run_power_analysis(
    data_clean = df,
    unit_var = "unit",
    group_var = "cohort",
    time_var = "time",
    rel_pass_var = "rel_pass",
    treat_ind_var = "treat_ind",
    outcome = "y",
    pta_type = "imputation",
    percent_effect = 0.10,
    n_sims = 1,
    noise_spec = list(engine = "iid"),
    models_to_run = "imputation"
  )

  expect_equal(unique(result$final_power$noise_engine), "iid")
  expect_equal(unique(result$final_power$obs_model), "gaussian")
})


# =============================================================================
# Test 6: design_resample stub — errors for cluster_bootstrap
# =============================================================================
test_that("design_resample='cluster_bootstrap' raises informative error", {
  df <- create_determinism_test_data()

  expect_error(
    run_power_analysis(
      data_clean = df,
      unit_var = "unit",
      group_var = "cohort",
      time_var = "time",
      rel_pass_var = "rel_pass",
      treat_ind_var = "treat_ind",
      outcome = "y",
      pta_type = "imputation",
      percent_effect = 0.10,
      n_sims = 1,
      noise_spec = list(engine = "none"),
      design_resample = "cluster_bootstrap",
      models_to_run = "imputation"
    ),
    "not yet implemented"
  )
})


# =============================================================================
# Test 7: Default noise_spec in run_power_analysis is engine="none"
# =============================================================================
test_that("run_power_analysis defaults to engine='none'", {
  df <- create_determinism_test_data()

  # Call without specifying noise_spec — should use default list(engine = "none")
  # and emit the n_sims guard message (since n_sims=100 default > 1)
  expect_message(
    result <- run_power_analysis(
      data_clean = df,
      unit_var = "unit",
      group_var = "cohort",
      time_var = "time",
      rel_pass_var = "rel_pass",
      treat_ind_var = "treat_ind",
      outcome = "y",
      pta_type = "imputation",
      percent_effect = 0.10,
      models_to_run = "imputation"
    ),
    "engine='none' produces deterministic data"
  )

  # Should have noise_engine = "none" in output
  expect_equal(unique(result$final_power$noise_engine), "none")
})
