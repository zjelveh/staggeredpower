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

#' Run Vanilla Poisson Event Study for Pre-trend Testing
#'
#' Runs a standard Poisson fixed-effects event study to extract
#' pre-treatment coefficients for testing multiplicative parallel trends.
#' This is used for ETWFE Poisson models, which don't produce pre-treatment
#' coefficients by design.
#'
#' @param data Data frame with panel data
#' @param outcome_var Name of outcome variable (count)
#' @param time_var Name of time variable
#' @param id_var Name of unit identifier
#' @param group_var Name of cohort/treatment timing variable
#' @param cluster_var Name of clustering variable for SEs
#' @param ref_period Reference period for event study (default -1)
#' @param min_event Minimum event time to include (default -10)
#' @param max_event Maximum event time to include (default 10)
#'
#' @return List with:
#'   \item{pre_coefs}{Named vector of pre-treatment coefficients}
#'   \item{pre_vcov}{Variance-covariance matrix for pre_coefs}
#'   \item{model}{The fitted fixest model object}
#'   \item{method}{String identifying the method used}
#'
#' @importFrom fixest fepois
#' @importFrom data.table as.data.table
#' @export
run_vanilla_poisson_es <- function(data, outcome_var, time_var, id_var,
                                    group_var, cluster_var, ref_period = -1,
                                    min_event = -10, max_event = 10) {

  dt <- data.table::as.data.table(data)

  # Create relative time (NA for never-treated coded as large negative)
  dt[, .rel_time := get(time_var) - get(group_var)]
  dt[is.na(.rel_time), .rel_time := -9999]

  # Trim to reasonable window
  dt[, .rel_time_trim := pmin(pmax(.rel_time, min_event), max_event)]
  dt[.rel_time == -9999, .rel_time_trim := -9999]

  # Run Poisson FE event study
  fml <- stats::as.formula(sprintf(
    "%s ~ i(.rel_time_trim, ref = c(%d, -9999)) | %s + %s",
    outcome_var, ref_period, id_var, time_var
  ))

  mod <- fixest::fepois(fml, data = dt,
                        vcov = stats::as.formula(paste0("~", cluster_var)))

  # Extract pre-treatment coefficients and vcov
  coef_names <- names(stats::coef(mod))
  pre_pattern <- "\\.rel_time_trim::-[0-9]+"
  pre_terms <- grep(pre_pattern, coef_names, value = TRUE)
  # Exclude reference period and never-treated indicator
  pre_terms <- pre_terms[!grepl("-9999", pre_terms)]

  pre_coefs <- stats::coef(mod)[pre_terms]
  pre_vcov <- stats::vcov(mod)[pre_terms, pre_terms, drop = FALSE]

  list(
    pre_coefs = pre_coefs,
    pre_vcov = pre_vcov,
    model = mod,
    method = "vanilla_poisson_event_study"
  )
}
