# R/pretrend_test.R
#' Pre-trend Testing Functions for Staggered DiD
#'
#' Functions for testing parallel trends assumptions in staggered
#' difference-in-differences designs.

#' Compute Wald Test on Pre-treatment Coefficients
#'
#' Performs a joint Wald test that all pre-treatment coefficients are zero,
#' using the full variance-covariance matrix.
#'
#' @param pre_coefs Named numeric vector of pre-treatment coefficients
#' @param pre_vcov Variance-covariance matrix for pre_coefs
#'
#' @return List with:
#'   \item{p_value}{P-value from chi-squared test}
#'   \item{wald_stat}{Wald test statistic}
#'   \item{df}{Degrees of freedom}
#'   \item{reject_at_05}{Logical, TRUE if p < 0.05}
#'   \item{warning}{NULL if successful, otherwise explanation of failure}
#'
#' @details
#' The Wald statistic is computed as W = beta' * Sigma^{-1} * beta,
#' which follows a chi-squared distribution with k degrees of freedom
#' under the null hypothesis that all coefficients are zero.
#'
#' @export
compute_pretrend_wald_test <- function(pre_coefs, pre_vcov) {


  # Edge case: not enough periods
  if (length(pre_coefs) < 1) {
    return(list(
      p_value = NA_real_,
      wald_stat = NA_real_,
      df = NA_integer_,
      reject_at_05 = NA,
      warning = "No pre-treatment periods available for testing"
    ))
  }

  # Edge case: missing values
  valid_idx <- !is.na(pre_coefs)
  if (sum(valid_idx) < 1) {
    return(list(
      p_value = NA_real_,
      wald_stat = NA_real_,
      df = NA_integer_,
      reject_at_05 = NA,
      warning = "All pre-treatment coefficients are NA"
    ))
  }

  # Subset to valid coefficients
  pre_coefs <- pre_coefs[valid_idx]
  pre_vcov <- pre_vcov[valid_idx, valid_idx, drop = FALSE]

  # Check if vcov is invertible
  vcov_inv <- tryCatch(
    solve(pre_vcov),
    error = function(e) NULL
  )

  if (is.null(vcov_inv)) {
    return(list(
      p_value = NA_real_,
      wald_stat = NA_real_,
      df = NA_integer_,
      reject_at_05 = NA,
      warning = "Variance-covariance matrix is singular"
    ))
  }

  # Wald statistic: W = beta' * Sigma^{-1} * beta ~ chi^2(k)
  wald_stat <- as.numeric(t(pre_coefs) %*% vcov_inv %*% pre_coefs)
  df <- length(pre_coefs)
  p_value <- 1 - pchisq(wald_stat, df = df)

  list(
    p_value = p_value,
    wald_stat = wald_stat,
    df = as.integer(df),
    reject_at_05 = p_value < 0.05,
    warning = NULL
  )
}
