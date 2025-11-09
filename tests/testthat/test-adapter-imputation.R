# tests/testthat/test-adapter-imputation.R
library(data.table)

# Helper function to generate realistic test data
make_test_data <- function(n_units = 40, n_periods = 15, seed = 123) {
  set.seed(seed)

  # Create panel structure
  test_data <- data.table(
    state = rep(1:n_units, each = n_periods),
    year = rep(2005:(2004 + n_periods), n_units)
  )

  # Assign treatment cohorts: 1/3 never treated, 1/3 treated in 2012, 1/3 treated in 2015
  cohort_assignment <- rep(c(0, 2012, 2015), length.out = n_units)
  test_data[, group := cohort_assignment[state]]

  # Create treated indicator
  test_data[, treated := as.integer(year >= group & group > 0)]

  # Generate outcome with unit and time fixed effects plus treatment effect
  test_data[, unit_fe := rnorm(1, mean = 50, sd = 10), by = state]
  test_data[, time_fe := rnorm(1, mean = 0, sd = 2), by = year]
  test_data[, outcome := unit_fe + time_fe + treated * 5 + rnorm(.N, mean = 0, sd = 3)]

  # Add a covariate for control tests
  test_data[, covariate1 := rnorm(.N)]

  test_data
}

test_that("imputation adapter translates parameters correctly", {
  skip_if_not_installed("didimputation")

  test_data <- make_test_data()

  adapter <- adapter_imputation()

  result <- adapter$fit(
    data = test_data,
    outcome_var = "outcome",
    time_var = "year",
    id_var = "state",
    group_var = "group",
    controls = NULL,
    cluster_var = "state"
  )

  # Result should have estimate and std.error
  expect_type(result$estimate, "double")
  expect_type(result$std.error, "double")
})

test_that("imputation adapter extraction produces standard format", {
  skip_if_not_installed("didimputation")

  test_data <- make_test_data()

  adapter <- adapter_imputation()
  raw_result <- adapter$fit(
    data = test_data,
    outcome_var = "outcome",
    time_var = "year",
    id_var = "state",
    group_var = "group",
    controls = NULL
  )

  std_result <- adapter$extract(raw_result)

  expect_s3_class(std_result, "standard_estimate")
  expect_type(std_result$agg$att, "double")
  expect_type(std_result$agg$se, "double")
  expect_equal(std_result$agg$model, "imputation")
})

test_that("imputation adapter requires didimputation package", {
  adapter <- adapter_imputation()
  expect_equal(adapter$requires, "didimputation")
  expect_true(check_adapter_deps(adapter))
})

test_that("imputation adapter handles controls", {
  skip_if_not_installed("didimputation")
  skip("didimputation has internal S4 error with controls on some data structures")

  test_data <- make_test_data()

  adapter <- adapter_imputation()

  # Should work with controls
  result <- adapter$fit(
    data = test_data,
    outcome_var = "outcome",
    time_var = "year",
    id_var = "state",
    group_var = "group",
    controls = c("covariate1")
  )

  expect_type(result$estimate, "double")
})
