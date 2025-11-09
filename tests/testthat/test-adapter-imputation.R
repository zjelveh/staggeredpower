# tests/testthat/test-adapter-imputation.R
library(data.table)

test_that("imputation adapter translates parameters correctly", {
  skip_if_not_installed("didimputation")

  test_data <- data.table(
    state = rep(1:10, each = 10),
    year = rep(2010:2019, 10),
    outcome = rnorm(100, mean = 50, sd = 10),
    group = rep(c(0, 2015, 2017), length.out = 100),
    treated = 0
  )
  test_data[year >= group & group > 0, treated := 1]

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

  test_data <- data.table(
    state = rep(1:10, each = 10),
    year = rep(2010:2019, 10),
    outcome = rnorm(100, mean = 50, sd = 10),
    group = rep(c(0, 2015, 2017), length.out = 100),
    treated = 0
  )
  test_data[year >= group & group > 0, treated := 1]

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

  test_data <- data.table(
    state = rep(1:10, each = 10),
    year = rep(2010:2019, 10),
    outcome = rnorm(100, mean = 50, sd = 10),
    group = rep(c(0, 2015, 2017), length.out = 100),
    treated = 0,
    covariate1 = rnorm(100)
  )
  test_data[year >= group & group > 0, treated := 1]

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
