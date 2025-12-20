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
