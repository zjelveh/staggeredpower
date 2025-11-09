# tests/testthat/test-adapter-cs.R
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

test_that("CS adapter translates parameters correctly", {
  skip_if_not_installed("did")

  test_data <- make_test_data()

  adapter <- adapter_cs()

  # Test basic fit with controls
  result <- adapter$fit(
    data = test_data,
    outcome_var = "outcome",
    time_var = "year",
    id_var = "state",
    group_var = "group",
    controls = c("covariate1"),
    cluster_var = "state",
    n_cores = 1
  )

  # Result should be a list with agg component
  expect_type(result, "list")
  expect_true("agg" %in% names(result))
})

test_that("CS adapter extraction produces standard format", {
  skip_if_not_installed("did")

  test_data <- make_test_data()

  adapter <- adapter_cs()
  raw_result <- adapter$fit(
    data = test_data,
    outcome_var = "outcome",
    time_var = "year",
    id_var = "state",
    group_var = "group",
    controls = NULL,
    n_cores = 1
  )

  # Extract to standard format
  std_result <- adapter$extract(raw_result)

  expect_s3_class(std_result, "standard_estimate")
  expect_type(std_result$agg$att, "double")
  expect_type(std_result$agg$se, "double")
  expect_equal(std_result$agg$model, "cs")
})

test_that("CS adapter requires did package", {
  adapter <- adapter_cs()
  expect_equal(adapter$requires, "did")

  # check_adapter_deps should pass if did is installed
  expect_true(check_adapter_deps(adapter))
})

test_that("CS adapter handles event study results", {
  skip_if_not_installed("did")

  test_data <- make_test_data()

  adapter <- adapter_cs()
  raw_result <- adapter$fit(
    data = test_data,
    outcome_var = "outcome",
    time_var = "year",
    id_var = "state",
    group_var = "group",
    controls = NULL,
    n_cores = 1,
    event_study = TRUE
  )

  std_result <- adapter$extract(raw_result)

  # Event study should be a data.table
  expect_s3_class(std_result$event_study, "data.table")
  expect_true("rel_time" %in% names(std_result$event_study))
  expect_true("att" %in% names(std_result$event_study))
})
