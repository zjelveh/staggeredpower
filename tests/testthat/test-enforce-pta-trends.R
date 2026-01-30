# tests/testthat/test-enforce-pta-trends.R
# Test cohort-specific trend options for enforce_PTA

library(data.table)
library(fixest)

# Helper function to create test data with known cohort-specific trends
create_trend_data <- function(n_units = 50, n_times = 10, n_cohorts = 3,
                               cohort_slopes = c(0, 0.5, -0.3, 0.2)) {
  # Create balanced panel
  df <- data.table(
    unit = rep(1:n_units, each = n_times),
    time = rep(1:n_times, n_units)
  )

  # Assign units to cohorts (staggered treatment at times 5, 6, 7)
  # Some units are never-treated
  cohort_times <- c(NA, 5, 6, 7)  # NA = never-treated, others = treatment cohort
  set.seed(42)
  df[, cohort := sample(cohort_times, 1), by = unit]

  # Create cohort_index for slope lookup (0=never-treated, 1/2/3=treated cohorts)
  df[, cohort_index := fifelse(is.na(cohort), 0L,
                                fifelse(cohort == 5, 1L,
                                         fifelse(cohort == 6, 2L, 3L)))]

  # Assign cohort slopes using sequential index
  df[, slope := cohort_slopes[cohort_index + 1]]

  # Generate outcome with:
  # - unit fixed effect
  # - time fixed effect
  # - cohort-specific linear trend
  # - noise
  set.seed(123)
  df[, unit_fe := rnorm(1, mean = 10, sd = 2), by = unit]
  df[, time_fe := (time - 5) * 0.1]  # Common time trend
  df[, cohort_trend := slope * (time - 1)]  # Cohort-specific trend
  df[, noise := rnorm(.N, mean = 0, sd = 0.5)]

  df[, outcome := unit_fe + time_fe + cohort_trend + noise]

  # Add treated flag
  df[, treated := !is.na(cohort) & time >= cohort]

  # Clean up helper columns (keep only what enforce_PTA expects)
  df_clean <- df[, .(unit, time, cohort, outcome)]

  return(list(
    data = df_clean,
    full_data = df,
    cohort_slopes = cohort_slopes
  ))
}

# Helper function to create Poisson test data
create_poisson_trend_data <- function(n_units = 50, n_times = 10,
                                       cohort_log_slopes = c(0, 0.05, -0.03, 0.02)) {
  df <- data.table(
    unit = rep(1:n_units, each = n_times),
    time = rep(1:n_times, n_units)
  )

  cohort_times <- c(NA, 5, 6, 7)
  set.seed(42)
  df[, cohort := sample(cohort_times, 1), by = unit]

  # Create cohort_index for slope lookup (0=never-treated, 1/2/3=treated cohorts)
  df[, cohort_index := fifelse(is.na(cohort), 0L,
                                fifelse(cohort == 5, 1L,
                                         fifelse(cohort == 6, 2L, 3L)))]
  df[, log_slope := cohort_log_slopes[cohort_index + 1]]

  set.seed(123)
  df[, pop := sample(10000:50000, 1), by = unit]  # Population
  df[, unit_fe := rnorm(1, mean = -10, sd = 0.5), by = unit]  # Log scale unit FE
  df[, time_fe := (time - 5) * 0.02]  # Log scale time FE
  df[, cohort_log_trend := log_slope * (time - 1)]

  # Generate Poisson counts
  df[, log_rate := unit_fe + time_fe + cohort_log_trend]
  df[, lambda := exp(log_rate) * pop]
  df[, count := rpois(.N, lambda)]
  df[, rate := count / pop * 100000]  # Rate per 100k

  df_clean <- df[, .(unit, time, cohort, count, rate, pop)]

  return(list(
    data = df_clean,
    full_data = df,
    cohort_log_slopes = cohort_log_slopes
  ))
}

# -----------------------------------------------------------------------------
# Test 1: Backward compatibility - default trend_type="common" unchanged
# -----------------------------------------------------------------------------
test_that("enforce_PTA_imputation default (common) behavior unchanged", {
  test_data <- create_trend_data()
  df <- test_data$data

  # Run with explicit trend_type="common"
  result_explicit <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "outcome",
    method = "imputation", seed = 42, trend_type = "common"
  )

  # Run without trend_type (should default to common)
  result_default <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "outcome",
    method = "imputation", seed = 42
  )

  # Results should be identical
  expect_equal(result_explicit$counterfactual, result_default$counterfactual)

  # Check that counterfactual column exists

  expect_true("counterfactual" %in% names(result_explicit))
  expect_false(any(is.na(result_explicit$counterfactual)))
})

# -----------------------------------------------------------------------------
# Test 2: Cohort-specific linear trends (trend_order=1)
# -----------------------------------------------------------------------------
test_that("enforce_PTA_imputation cohort_trend with linear trends works", {
  test_data <- create_trend_data()
  df <- test_data$data

  result <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "outcome",
    method = "imputation", seed = 42,
    trend_type = "cohort_trend", trend_order = 1
  )

  # Check basic properties
  expect_true("counterfactual" %in% names(result))
  expect_false(any(is.na(result$counterfactual)))

  # Counterfactuals should differ from common trend approach
  result_common <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "outcome",
    method = "imputation", seed = 42, trend_type = "common"
  )

  # For treated observations, counterfactuals may differ
  treated_mask <- !is.na(df$cohort) & df$time >= df$cohort
  # At least some difference expected (unless by chance they're identical)
  # Just verify both run without error
  expect_equal(nrow(result), nrow(result_common))
})

# -----------------------------------------------------------------------------
# Test 3: Cohort-specific quadratic trends (trend_order=2)
# -----------------------------------------------------------------------------
test_that("enforce_PTA_imputation cohort_trend with quadratic trends works", {
  test_data <- create_trend_data()
  df <- test_data$data

  result <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "outcome",
    method = "imputation", seed = 42,
    trend_type = "cohort_trend", trend_order = 2
  )

  expect_true("counterfactual" %in% names(result))
  expect_false(any(is.na(result$counterfactual)))
  expect_equal(nrow(result), nrow(df))
})

# -----------------------------------------------------------------------------
# Test 4: Poisson method with cohort-specific trends
# -----------------------------------------------------------------------------
test_that("enforce_PTA_poisson cohort_trend works", {
  test_data <- create_poisson_trend_data()
  df <- test_data$data

  # Test with rate outcome
  result_rate <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "rate",
    method = "poisson", seed = 42,
    pop_var = "pop", outcome_type = "rate",
    trend_type = "cohort_trend", trend_order = 1
  )

  expect_true("counterfactual" %in% names(result_rate))
  expect_false(any(is.na(result_rate$counterfactual)))
  expect_true(all(result_rate$counterfactual >= 0))  # Rates should be non-negative

  # Test with count outcome
  result_count <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "count",
    method = "poisson", seed = 42,
    pop_var = "pop", outcome_type = "count",
    trend_type = "cohort_trend", trend_order = 1
  )

  expect_true("counterfactual" %in% names(result_count))
  expect_false(any(is.na(result_count$counterfactual)))
  expect_true(all(result_count$counterfactual >= 0))  # Counts should be non-negative
})

# -----------------------------------------------------------------------------
# Test 5: CS method ignores trend_type (no error)
# -----------------------------------------------------------------------------
test_that("enforce_PTA CS method ignores trend_type without error", {
  test_data <- create_trend_data()
  df <- test_data$data

  # CS method should work with trend_type specified (but ignore it)
  expect_no_error({
    result <- enforce_PTA(
      df, unit = "unit", group = "cohort", time = "time", outcome = "outcome",
      method = "CS", seed = 42,
      trend_type = "cohort_trend", trend_order = 2
    )
  })
})

# -----------------------------------------------------------------------------
# Test 6: Invalid trend_order validation
# -----------------------------------------------------------------------------
test_that("trend_order validation works", {
  test_data <- create_trend_data()
  df <- test_data$data

  # trend_order < 1 with cohort_trend should error
  expect_error(
    enforce_PTA(
      df, unit = "unit", group = "cohort", time = "time", outcome = "outcome",
      method = "imputation", trend_type = "cohort_trend", trend_order = 0
    ),
    "trend_order must be >= 1"
  )
})

# -----------------------------------------------------------------------------
# Test 7: trend_order ignored for common trend_type
# -----------------------------------------------------------------------------
test_that("trend_order ignored when trend_type='common'", {
  test_data <- create_trend_data()
  df <- test_data$data

  result1 <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "outcome",
    method = "imputation", seed = 42, trend_type = "common", trend_order = 1
  )

  result2 <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "outcome",
    method = "imputation", seed = 42, trend_type = "common", trend_order = 5
  )

  # Should be identical since trend_order is ignored for common
  expect_equal(result1$counterfactual, result2$counterfactual)
})

# -----------------------------------------------------------------------------
# Test 8: enforce_PTA function signature includes new parameters
# -----------------------------------------------------------------------------
test_that("enforce_PTA has trend_type and trend_order parameters", {
  params <- names(formals(enforce_PTA))

  expect_true("trend_type" %in% params)
  expect_true("trend_order" %in% params)
})

# -----------------------------------------------------------------------------
# Test 9: run_power_analysis has trend_type and trend_order parameters
# -----------------------------------------------------------------------------
test_that("run_power_analysis has trend_type and trend_order parameters", {
  params <- names(formals(run_power_analysis))

  expect_true("trend_type" %in% params)
  expect_true("trend_order" %in% params)
})

# -----------------------------------------------------------------------------
# Test 10: Cohort trends extrapolate to post-treatment periods
# -----------------------------------------------------------------------------
test_that("cohort_trend extrapolates trends to post-treatment", {
  # Create simple data where we can verify extrapolation
  df <- data.table(
    unit = rep(1:20, each = 10),
    time = rep(1:10, 20)
  )

  # Half units treated at time 6
  df[, cohort := fifelse(unit <= 10, 6L, NA_integer_)]

  # Create outcome with clear cohort-specific trend for treated cohort
  # Treated units (cohort=6) have slope +1 per time period
  # Never-treated have slope 0
  df[, outcome := 10 + fifelse(is.na(cohort), 0, (time - 1) * 1.0) + rnorm(.N, 0, 0.01)]

  result <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "outcome",
    method = "imputation", seed = 42,
    trend_type = "cohort_trend", trend_order = 1
  )

  # For treated units at post-treatment times, counterfactual should continue the trend
  # (extrapolate beyond pre-treatment period)
  treated_post <- result[!is.na(cohort) & time >= cohort]

  # The counterfactuals should exist and be reasonable (roughly following the pre-trend)
  expect_true(nrow(treated_post) > 0)
  expect_false(any(is.na(treated_post$counterfactual)))
})

# -----------------------------------------------------------------------------
# Test 11: With controls - imputation method
# -----------------------------------------------------------------------------
test_that("enforce_PTA_imputation cohort_trend works with controls", {
  test_data <- create_trend_data()
  df <- test_data$data

  # Add a control variable
  set.seed(456)
  df[, control1 := rnorm(.N)]
  df[, outcome := outcome + 0.5 * control1]  # Make control matter

  result <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "outcome",
    controls = "control1",
    method = "imputation", seed = 42,
    trend_type = "cohort_trend", trend_order = 1
  )

  expect_true("counterfactual" %in% names(result))
  expect_false(any(is.na(result$counterfactual)))
})

# -----------------------------------------------------------------------------
# Test 12: With controls - Poisson method
# -----------------------------------------------------------------------------
test_that("enforce_PTA_poisson cohort_trend works with controls", {
  test_data <- create_poisson_trend_data()
  df <- test_data$data

  # Add a control variable
  set.seed(456)
  df[, control1 := rnorm(.N)]

  result <- enforce_PTA(
    df, unit = "unit", group = "cohort", time = "time", outcome = "rate",
    controls = "control1",
    method = "poisson", seed = 42,
    pop_var = "pop", outcome_type = "rate",
    trend_type = "cohort_trend", trend_order = 1
  )

  expect_true("counterfactual" %in% names(result))
  expect_false(any(is.na(result$counterfactual)))
})
