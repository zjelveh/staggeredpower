# tests/testthat/test-adapter-cs.R
library(data.table)

test_that("CS adapter translates parameters correctly", {
  skip_if_not_installed("did")

  # Setup test data (minimal staggered DiD panel)
  test_data <- data.table(
    state = rep(1:10, each = 10),
    year = rep(2010:2019, 10),
    outcome = rnorm(100, mean = 50, sd = 10),
    group = rep(c(0, 2015, 2017), length.out = 100),
    treated = 0
  )
  test_data[year >= group & group > 0, treated := 1]

  adapter <- adapter_cs()

  # Test basic fit with controls
  result <- adapter$fit(
    data = test_data,
    outcome_var = "outcome",
    time_var = "year",
    id_var = "state",
    group_var = "group",
    controls = c("treated"),  # Minimal control
    cluster_var = "state",
    n_cores = 1
  )

  # Result should be a list with agg component
  expect_type(result, "list")
  expect_true("agg" %in% names(result))
})

test_that("CS adapter extraction produces standard format", {
  skip_if_not_installed("did")

  # Setup and fit
  test_data <- data.table(
    state = rep(1:10, each = 10),
    year = rep(2010:2019, 10),
    outcome = rnorm(100, mean = 50, sd = 10),
    group = rep(c(0, 2015, 2017), length.out = 100),
    treated = 0
  )
  test_data[year >= group & group > 0, treated := 1]

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

  test_data <- data.table(
    state = rep(1:10, each = 10),
    year = rep(2010:2019, 10),
    outcome = rnorm(100, mean = 50, sd = 10),
    group = rep(c(0, 2015, 2017), length.out = 100),
    treated = 0
  )
  test_data[year >= group & group > 0, treated := 1]

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
