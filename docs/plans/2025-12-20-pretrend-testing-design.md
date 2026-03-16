# Pre-trend Testing Feature Design

**Date:** 2025-12-20
**Status:** Approved
**Author:** Claude + Zubin Jelveh

## Overview

Add formal parallel trends testing to all estimator adapters in `staggeredpower`, with special handling for Poisson models that includes both multiplicative PT testing and a diagnostic comparing additive vs multiplicative PT stability.

### Use Cases

1. **Power analysis:** Verify that multiplicative PT holds in simulated data (sanity check that `enforce_PTA_poisson()` worked correctly)
2. **Applied research:** Test whether multiplicative PT is plausible before running ETWFE Poisson

### Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Behavior on rejection | Warning only | Pre-trend tests are informative but shouldn't be gatekeepers; low power issues |
| CV diagnostic scope | Poisson models only | Answers "additive vs multiplicative?" which is the key question for count data |
| Integration approach | Opt-in via `pretrend_test = TRUE` | Avoids extra computation when not needed |
| ETWFE transparency | Explicit about using vanilla Poisson | Users should understand their diagnostics |
| Output location | `metadata$pretrend_test` | Keeps core `standard_estimate` structure unchanged (backward compatible) |
| Error handling | Return NA with warning | Graceful degradation when test can't run |
| Variance-covariance | Full matrix (not diagonal) | Rigorous Wald test accounting for correlation |

## Interface

All adapter `fit_fn` functions gain one new parameter:

```r
fit_fn <- function(...,
                   pretrend_test = FALSE,  # NEW
                   ...)
```

### Output Structure

When `pretrend_test = TRUE`, results are added to `metadata$pretrend_test`:

```r
metadata$pretrend_test <- list(
  p_value = 0.079,
  wald_stat = 15.46,
  df = 9,
  reject_at_05 = FALSE,
  method = "event_study",
  # or "vanilla_poisson_event_study" for ETWFE Poisson
  warning = NULL,  # or explanation if test couldn't run

  # Poisson only:
  cv_comparison = list(
    ratio_cv = 0.197,
    diff_cv = 4.293,
    recommendation = "multiplicative"
  )
)
```

## Implementation

### New File: `R/pretrend_test.R`

Contains three functions:

#### 1. `compute_pretrend_wald_test()`

Computes Wald test on pre-treatment coefficients using full variance-covariance matrix.

```r
#' Compute Wald Test on Pre-treatment Coefficients (Full VCV)
#'
#' @param pre_coefs Named vector of pre-treatment coefficients
#' @param pre_vcov Variance-covariance matrix for pre_coefs
#' @return List with p_value, wald_stat, df, reject_at_05, warning
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
    df = df,
    reject_at_05 = p_value < 0.05,
    warning = NULL
  )
}
```

#### 2. `run_vanilla_poisson_es()`

Runs vanilla Poisson event study for ETWFE pre-trend testing.

```r
#' Run Vanilla Poisson Event Study for Pre-trend Testing
#'
#' @param data Data frame with panel data
#' @param outcome_var Name of outcome variable
#' @param time_var Name of time variable
#' @param id_var Name of unit identifier
#' @param group_var Name of cohort variable
#' @param cluster_var Name of cluster variable
#' @param ref_period Reference period (default -1)
#' @return List with pre_coefs, pre_vcov, model, method
run_vanilla_poisson_es <- function(data, outcome_var, time_var, id_var,
                                    group_var, cluster_var, ref_period = -1) {

  dt <- data.table::as.data.table(data)

  # Create relative time (NA for never-treated coded as large negative)
  dt[, .rel_time := get(time_var) - get(group_var)]
  dt[is.na(.rel_time), .rel_time := -9999]

  # Trim to reasonable window
  dt[, .rel_time_trim := pmin(pmax(.rel_time, -10), 10)]
  dt[.rel_time == -9999, .rel_time_trim := -9999]

  # Run Poisson FE event study
  fml <- as.formula(sprintf(
    "%s ~ i(.rel_time_trim, ref = c(%d, -9999)) | %s + %s",
    outcome_var, ref_period, id_var, time_var
  ))

  mod <- fixest::fepois(fml, data = dt,
                        vcov = as.formula(paste0("~", cluster_var)))

  # Extract pre-treatment coefficients and vcov
  coef_names <- names(coef(mod))
  pre_pattern <- "\\.rel_time_trim::-[0-9]+"
  pre_terms <- grep(pre_pattern, coef_names, value = TRUE)
  pre_terms <- pre_terms[!grepl("-9999", pre_terms)]

  list(
    pre_coefs = coef(mod)[pre_terms],
    pre_vcov = vcov(mod)[pre_terms, pre_terms, drop = FALSE],
    model = mod,
    method = "vanilla_poisson_event_study"
  )
}
```

#### 3. `compute_cv_comparison()`

Compares stability of additive vs multiplicative parallel trends.

```r
#' Compare Stability of Additive vs Multiplicative PT
#'
#' @param data Panel data
#' @param outcome_var Outcome variable name
#' @param time_var, group_var, id_var Column names
#' @return List with ratio_cv, diff_cv, recommendation
compute_cv_comparison <- function(data, outcome_var, time_var, group_var, id_var) {

  dt <- data.table::as.data.table(data)

  # Identify early vs late adopters
  cohorts <- dt[!is.na(get(group_var)), unique(get(group_var))]
  median_cohort <- median(cohorts)

  dt[, .group := fifelse(get(group_var) <= median_cohort, "early", "late")]

  # Focus on pre-treatment periods (before earliest treatment)
  min_cohort <- min(cohorts, na.rm = TRUE)
  pre_data <- dt[get(time_var) < min_cohort]

  # Compute means by year and group
  trends <- pre_data[, .(mean_y = mean(get(outcome_var), na.rm = TRUE)),
                      by = c(time_var, ".group")]
  trends_wide <- data.table::dcast(trends,
                                    as.formula(paste(time_var, "~ .group")),
                                    value.var = "mean_y")

  trends_wide[, difference := early - late]
  trends_wide[, ratio := early / late]

  # Compute CVs (coefficient of variation)
  diff_cv <- sd(trends_wide$difference, na.rm = TRUE) /
             abs(mean(trends_wide$difference, na.rm = TRUE))
  ratio_cv <- sd(trends_wide$ratio, na.rm = TRUE) /
              mean(trends_wide$ratio, na.rm = TRUE)

  list(
    ratio_cv = ratio_cv,
    diff_cv = diff_cv,
    recommendation = if (ratio_cv < diff_cv) "multiplicative" else "additive"
  )
}
```

### Adapter Changes

#### VCV Extraction by Package

| Adapter | How to extract pre-treatment VCV |
|---------|----------------------------------|
| CS | `did::aggte(mod, type="dynamic")` influence functions → reconstruct VCV |
| did2s | `vcov(result_es)` on fixest object, subset to pre-treatment terms |
| imputation | `vcov(result)` on fixest object |
| ETWFE Poisson | Use `run_vanilla_poisson_es()`, then `vcov(mod)` |

#### did2s Example Integration

```r
# In adapter_did2s.R fit_fn, after event study...
if (pretrend_test && !is.null(result_es)) {
  full_vcov <- vcov(result_es)
  pre_terms <- grep("rel_time.*::-[0-9]+", rownames(full_vcov), value = TRUE)
  pre_terms <- pre_terms[!grepl("::-1$", pre_terms)]  # exclude reference

  pt_result <- compute_pretrend_wald_test(
    coef(result_es)[pre_terms],
    full_vcov[pre_terms, pre_terms]
  )
  metadata$pretrend_test <- pt_result
  metadata$pretrend_test$method <- "event_study"
}
```

#### ETWFE Poisson Integration

```r
# In adapter_etwfe.R fit_fn...
if (pretrend_test && !is.null(family) && family == "poisson") {
  message("Note: ETWFE does not produce pre-treatment coefficients by design.\n",
          "Running supplementary vanilla Poisson event study for pre-trend test.")

  # Run vanilla Poisson
  vanilla_result <- run_vanilla_poisson_es(
    data, outcome_var, time_var, id_var, group_var, cluster_var
  )

  # Compute Wald test
  pt_result <- compute_pretrend_wald_test(
    vanilla_result$pre_coefs,
    vanilla_result$pre_vcov
  )
  pt_result$method <- "vanilla_poisson_event_study"

  # Compute CV comparison
  cv_result <- compute_cv_comparison(data, outcome_var, time_var, group_var, id_var)
  pt_result$cv_comparison <- cv_result

  metadata$pretrend_test <- pt_result
}
```

## Testing

### Unit Tests (`tests/testthat/test-pretrend.R`)

```r
test_that("compute_pretrend_wald_test returns correct structure", {
  pre_coefs <- c(0.1, 0.05, -0.02)
  pre_vcov <- diag(c(0.01, 0.01, 0.01))

  result <- compute_pretrend_wald_test(pre_coefs, pre_vcov)

  expect_named(result, c("p_value", "wald_stat", "df", "reject_at_05", "warning"))
  expect_equal(result$df, 3)
  expect_true(result$p_value >= 0 && result$p_value <= 1)
})

test_that("pretrend test handles edge cases", {
  # Empty coefficients

  result <- compute_pretrend_wald_test(numeric(0), matrix(nrow=0, ncol=0))
  expect_true(is.na(result$p_value))
  expect_false(is.null(result$warning))

  # Singular vcov
  pre_coefs <- c(0.1, 0.1)
  singular_vcov <- matrix(c(1, 1, 1, 1), nrow = 2)
  result <- compute_pretrend_wald_test(pre_coefs, singular_vcov)
  expect_true(is.na(result$p_value))
})

test_that("ETWFE Poisson runs vanilla Poisson for pretrend test", {
  skip_if_not_installed("etwfe")
  adapter <- adapter_etwfe_poisson()
  result <- adapter$fit(..., pretrend_test = TRUE)

  expect_true("pretrend_test" %in% names(result$metadata))
  expect_equal(result$metadata$pretrend_test$method, "vanilla_poisson_event_study")
  expect_true("cv_comparison" %in% names(result$metadata$pretrend_test))
})

test_that("CV comparison identifies correct PT assumption", {
  # Create synthetic data where ratio is more stable than difference
  # Verify recommendation = "multiplicative"
})
```

## File Changes Summary

| File | Change |
|------|--------|
| `R/pretrend_test.R` | **NEW** - Core functions |
| `R/adapter_cs.R` | Add `pretrend_test` param + integration |
| `R/adapter_did2s.R` | Add `pretrend_test` param + integration |
| `R/adapter_imputation.R` | Add `pretrend_test` param + integration |
| `R/adapter_etwfe.R` | Add `pretrend_test` param + vanilla Poisson + CV comparison |
| `tests/testthat/test-pretrend.R` | **NEW** - Unit tests |
| `NAMESPACE` | Export new functions |

## Dependencies

No new package dependencies required. Uses existing:
- `fixest` (for `fepois`, `vcov`)
- `data.table` (for data manipulation)
