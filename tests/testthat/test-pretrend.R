# tests/testthat/test-pretrend.R

test_that("compute_pretrend_wald_test returns correct structure", {
  # Simple diagonal vcov (independent coefficients)
  pre_coefs <- c(a = 0.1, b = 0.05, c = -0.02)
  pre_vcov <- diag(c(0.01, 0.01, 0.01))
  rownames(pre_vcov) <- colnames(pre_vcov) <- names(pre_coefs)

  result <- compute_pretrend_wald_test(pre_coefs, pre_vcov)

  expect_type(result, "list")
  expect_named(result, c("p_value", "wald_stat", "df", "reject_at_05", "warning"))
  expect_equal(result$df, 3)
  expect_true(result$p_value >= 0 && result$p_value <= 1)
  expect_null(result$warning)
})

test_that("compute_pretrend_wald_test handles empty coefficients", {
  result <- compute_pretrend_wald_test(numeric(0), matrix(nrow = 0, ncol = 0))

  expect_true(is.na(result$p_value))
  expect_true(is.na(result$wald_stat))
  expect_true(is.na(result$df))
  expect_false(is.null(result$warning))
  expect_match(result$warning, "No pre-treatment")
})

test_that("compute_pretrend_wald_test handles NA coefficients", {
  pre_coefs <- c(a = NA_real_, b = NA_real_)
  pre_vcov <- diag(2)

  result <- compute_pretrend_wald_test(pre_coefs, pre_vcov)

  expect_true(is.na(result$p_value))
  expect_match(result$warning, "NA")
})

test_that("compute_pretrend_wald_test handles singular vcov", {
  pre_coefs <- c(a = 0.1, b = 0.1)
  singular_vcov <- matrix(c(1, 1, 1, 1), nrow = 2)
  rownames(singular_vcov) <- colnames(singular_vcov) <- names(pre_coefs)

  result <- compute_pretrend_wald_test(pre_coefs, singular_vcov)

  expect_true(is.na(result$p_value))
  expect_match(result$warning, "singular")
})

test_that("compute_pretrend_wald_test computes correct Wald statistic", {
  # Known example: beta = (1, 2), Sigma = I
  # Wald = beta' * Sigma^{-1} * beta = 1^2 + 2^2 = 5
  pre_coefs <- c(a = 1, b = 2)
  pre_vcov <- diag(2)
  rownames(pre_vcov) <- colnames(pre_vcov) <- names(pre_coefs)

  result <- compute_pretrend_wald_test(pre_coefs, pre_vcov)

  expect_equal(result$wald_stat, 5)
  expect_equal(result$df, 2)
  # p-value for chi2(2) > 5 is about 0.082
  expect_true(abs(result$p_value - 0.0821) < 0.01)
  expect_false(result$reject_at_05)
})

test_that("run_vanilla_poisson_es returns correct structure", {
  skip_if_not_installed("fixest")

  # Create minimal test data
  set.seed(123)
  n_units <- 10
  n_periods <- 10
  test_data <- data.table::data.table(
    unit_id = rep(1:n_units, each = n_periods),
    year = rep(2000:(2000 + n_periods - 1), n_units),
    cohort = rep(c(2005, 2005, 2006, 2006, 2007, NA, NA, NA, NA, NA), each = n_periods),
    y = rpois(n_units * n_periods, lambda = 5)
  )

  result <- run_vanilla_poisson_es(
    data = test_data,
    outcome_var = "y",
    time_var = "year",
    id_var = "unit_id",
    group_var = "cohort",
    cluster_var = "unit_id"
  )

  expect_type(result, "list")
  expect_named(result, c("pre_coefs", "pre_vcov", "model", "method"))
  expect_equal(result$method, "vanilla_poisson_event_study")
  expect_true(inherits(result$model, "fixest"))

  # pre_coefs and pre_vcov should have matching dimensions
  expect_equal(length(result$pre_coefs), nrow(result$pre_vcov))
  expect_equal(length(result$pre_coefs), ncol(result$pre_vcov))
})

test_that("run_vanilla_poisson_es extracts pre-treatment coefficients only", {
  skip_if_not_installed("fixest")

  # Create data with clear pre/post structure
  set.seed(456)
  n_units <- 20
  n_periods <- 15
  test_data <- data.table::data.table(
    unit_id = rep(1:n_units, each = n_periods),
    year = rep(2000:(2000 + n_periods - 1), n_units),
    cohort = rep(c(rep(2008, 10), rep(NA, 10)), each = n_periods),
    y = rpois(n_units * n_periods, lambda = 3)
  )

  result <- run_vanilla_poisson_es(
    data = test_data,
    outcome_var = "y",
    time_var = "year",
    id_var = "unit_id",
    group_var = "cohort",
    cluster_var = "unit_id",
    ref_period = -1
  )

  # All extracted coefficients should be for negative relative times
  # (excluding reference period -1)
  coef_names <- names(result$pre_coefs)
  expect_true(all(grepl("::-[0-9]+$", coef_names)))
  expect_false(any(grepl("::-1$", coef_names)))  # ref period excluded
})

test_that("compute_cv_comparison returns correct structure", {
  # Create test data where ratio is more stable than difference
  set.seed(789)
  test_data <- data.table::data.table(
    unit_id = rep(1:20, each = 10),
    year = rep(1990:1999, 20),
    cohort = rep(c(rep(2005, 10), rep(2010, 10)), each = 10),
    y = c(
      # Early adopters: declining from 100 to 50
      rep(seq(100, 50, length.out = 10), 10),
      # Late adopters: declining from 50 to 25 (same 50% decline = stable ratio)
      rep(seq(50, 25, length.out = 10), 10)
    ) + rnorm(200, 0, 2)
  )

  result <- compute_cv_comparison(
    data = test_data,
    outcome_var = "y",
    time_var = "year",
    group_var = "cohort",
    id_var = "unit_id"
  )

  expect_type(result, "list")
  expect_named(result, c("ratio_cv", "diff_cv", "recommendation"))
  expect_true(result$ratio_cv >= 0)
  expect_true(result$diff_cv >= 0)
  expect_true(result$recommendation %in% c("multiplicative", "additive"))
})

test_that("compute_cv_comparison recommends multiplicative when ratio more stable", {
  # Construct data where ratio is constant but difference varies
  test_data <- data.table::data.table(
    unit_id = rep(1:20, each = 5),
    year = rep(1995:1999, 20),
    cohort = rep(c(rep(2005, 10), rep(2010, 10)), each = 5),
    y = c(
      # Early: 100, 80, 60, 40, 20
      rep(c(100, 80, 60, 40, 20), 10),
      # Late: 50, 40, 30, 20, 10 (ratio = 2 always, diff shrinks)
      rep(c(50, 40, 30, 20, 10), 10)
    )
  )

  result <- compute_cv_comparison(
    data = test_data,
    outcome_var = "y",
    time_var = "year",
    group_var = "cohort",
    id_var = "unit_id"
  )

  # Ratio should be more stable (lower CV)
  expect_true(result$ratio_cv < result$diff_cv)
  expect_equal(result$recommendation, "multiplicative")
})

test_that("compute_cv_comparison recommends additive when difference more stable", {
  # Construct data where difference is constant but ratio varies
  test_data <- data.table::data.table(
    unit_id = rep(1:20, each = 5),
    year = rep(1995:1999, 20),
    cohort = rep(c(rep(2005, 10), rep(2010, 10)), each = 5),
    y = c(
      # Early: 60, 70, 80, 90, 100
      rep(c(60, 70, 80, 90, 100), 10),
      # Late: 50, 60, 70, 80, 90 (diff = 10 always, ratio varies)
      rep(c(50, 60, 70, 80, 90), 10)
    )
  )

  result <- compute_cv_comparison(
    data = test_data,
    outcome_var = "y",
    time_var = "year",
    group_var = "cohort",
    id_var = "unit_id"
  )

  # Difference should be more stable (lower CV)
  expect_true(result$diff_cv < result$ratio_cv)
  expect_equal(result$recommendation, "additive")
})

test_that("did2s adapter returns pretrend_test when requested", {
  skip_if_not_installed("did2s")

  # Create test data
  set.seed(111)
  n_units <- 30
  n_periods <- 12
  test_data <- data.table::data.table(
    unit_id = rep(1:n_units, each = n_periods),
    year = rep(2000:(2000 + n_periods - 1), n_units),
    cohort = rep(c(rep(2006, 15), rep(2009, 15)), each = n_periods),
    y = rnorm(n_units * n_periods, mean = 10, sd = 2)
  )

  adapter <- adapter_did2s()
  result <- adapter$fit(
    data = test_data,
    outcome_var = "y",
    time_var = "year",
    id_var = "unit_id",
    group_var = "cohort",
    event_study = TRUE,
    pretrend_test = TRUE
  )

  # Check pretrend_test exists in metadata
  expect_true("pretrend_test" %in% names(result$metadata))

  pt <- result$metadata$pretrend_test
  expect_named(pt, c("p_value", "wald_stat", "df", "reject_at_05", "warning", "method"))
  expect_equal(pt$method, "event_study")

  # p_value should be valid (might be NA if not enough pre-periods)
  if (!is.na(pt$p_value)) {
    expect_true(pt$p_value >= 0 && pt$p_value <= 1)
  }
})

test_that("did2s adapter skips pretrend_test when pretrend_test = FALSE", {
  skip_if_not_installed("did2s")

  set.seed(222)
  n_units <- 20
  n_periods <- 10
  test_data <- data.table::data.table(
    unit_id = rep(1:n_units, each = n_periods),
    year = rep(2000:(2000 + n_periods - 1), n_units),
    cohort = rep(c(rep(2005, 10), rep(2007, 10)), each = n_periods),
    y = rnorm(n_units * n_periods, mean = 10, sd = 2)
  )

  adapter <- adapter_did2s()
  result <- adapter$fit(
    data = test_data,
    outcome_var = "y",
    time_var = "year",
    id_var = "unit_id",
    group_var = "cohort",
    event_study = TRUE,
    pretrend_test = FALSE
  )

  # pretrend_test should NOT be in metadata
  expect_false("pretrend_test" %in% names(result$metadata))
})

test_that("did2s adapter handles pretrend_test = TRUE with event_study = FALSE", {
  skip_if_not_installed("did2s")

  set.seed(333)
  n_units <- 20
  n_periods <- 10
  test_data <- data.table::data.table(
    unit_id = rep(1:n_units, each = n_periods),
    year = rep(2000:(2000 + n_periods - 1), n_units),
    cohort = rep(c(rep(2005, 10), rep(2007, 10)), each = n_periods),
    y = rnorm(n_units * n_periods, mean = 10, sd = 2)
  )

  adapter <- adapter_did2s()

  # This should not error - it should handle gracefully with a warning
  result <- adapter$fit(
    data = test_data,
    outcome_var = "y",
    time_var = "year",
    id_var = "unit_id",
    group_var = "cohort",
    event_study = FALSE,
    pretrend_test = TRUE
  )

  # Check pretrend_test exists in metadata with warning
  expect_true("pretrend_test" %in% names(result$metadata))

  pt <- result$metadata$pretrend_test
  expect_named(pt, c("p_value", "wald_stat", "df", "reject_at_05", "warning", "method"))

  # Should have NA values and a warning
  expect_true(is.na(pt$p_value))
  expect_true(is.na(pt$wald_stat))
  expect_true(is.na(pt$df))
  expect_match(pt$warning, "requires event_study = TRUE")
  expect_equal(pt$method, "event_study")
})
