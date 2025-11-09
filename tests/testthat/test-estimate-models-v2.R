# tests/testthat/test-estimate-models-v2.R
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

test_that("estimate_models_v2 dispatches to correct adapter", {
  skip_if_not_installed("did")

  test_data <- make_test_data()

  # Run CS model
  results <- estimate_models_v2(
    data = test_data,
    id_var = "state",
    outcome_var = "outcome",
    time_var = "year",
    group_var = "group",
    models_to_run = "cs"
  )

  expect_type(results, "list")
  expect_true("cs" %in% names(results))
  expect_s3_class(results$cs, "standard_estimate")
})

test_that("estimate_models_v2 runs multiple models", {
  skip_if_not_installed("did")
  skip_if_not_installed("didimputation")

  test_data <- make_test_data()

  results <- estimate_models_v2(
    data = test_data,
    id_var = "state",
    outcome_var = "outcome",
    time_var = "year",
    group_var = "group",
    models_to_run = c("cs", "imputation")
  )

  expect_length(results, 2)
  expect_true(all(c("cs", "imputation") %in% names(results)))
  expect_s3_class(results$cs, "standard_estimate")
  expect_s3_class(results$imputation, "standard_estimate")
})

test_that("estimate_models_v2 throws error for unknown model", {
  test_data <- make_test_data(n_units = 5, n_periods = 5)

  expect_error(
    estimate_models_v2(
      data = test_data,
      id_var = "state",
      outcome_var = "outcome",
      time_var = "year",
      group_var = "group",
      models_to_run = "nonexistent_model"
    ),
    "No adapter registered"
  )
})

test_that("estimate_models_v2 passes controls correctly", {
  skip_if_not_installed("did")

  test_data <- make_test_data()

  # Should not throw error with controls (using CS which handles controls well)
  results <- estimate_models_v2(
    data = test_data,
    id_var = "state",
    outcome_var = "outcome",
    time_var = "year",
    group_var = "group",
    controls = c("covariate1"),
    models_to_run = "cs"
  )

  expect_s3_class(results$cs, "standard_estimate")
})

test_that("estimate_models_v2 handles event study requests", {
  skip_if_not_installed("did")

  test_data <- make_test_data()

  results <- estimate_models_v2(
    data = test_data,
    id_var = "state",
    outcome_var = "outcome",
    time_var = "year",
    group_var = "group",
    models_to_run = "cs",
    event_study = TRUE
  )

  expect_s3_class(results$cs, "standard_estimate")
  expect_s3_class(results$cs$event_study, "data.table")
  expect_true("rel_time" %in% names(results$cs$event_study))
  expect_true("att" %in% names(results$cs$event_study))
})
