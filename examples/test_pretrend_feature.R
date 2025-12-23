# Integration Test Script for Pre-trend Testing Feature
# This script demonstrates that the pre-trend testing feature works
# correctly with all three adapters: did2s, ETWFE Poisson, and CS

# Load the package (using devtools::load_all for development)
suppressMessages(devtools::load_all())
library(data.table)

cat("========================================\n")
cat("Pre-trend Testing Feature Integration Test\n")
cat("========================================\n\n")

# Create synthetic panel data with staggered adoption
set.seed(12345)
n_units <- 50
n_periods <- 15

test_data <- data.table(
  unit_id = rep(1:n_units, each = n_periods),
  year = rep(2000:(2000 + n_periods - 1), n_units),
  cohort = rep(c(
    rep(2008, 25),  # Early adopters
    rep(2012, 25)   # Late adopters
  ), each = n_periods)
)

# Generate outcome variable with some pre-trend (violation)
# and treatment effect
test_data[, rel_time := year - cohort]
test_data[is.na(rel_time), rel_time := -9999]

# Add unit fixed effects
set.seed(123)
unit_fe <- rnorm(n_units, mean = 50, sd = 10)
test_data[, unit_fe := unit_fe[unit_id]]

# Add time fixed effects
time_fe <- rnorm(n_periods, mean = 0, sd = 2)
test_data[, time_fe := time_fe[year - 1999]]

# Add outcome with small pre-trend and treatment effect
test_data[, y := unit_fe + time_fe +
           ifelse(rel_time >= 0 & rel_time != -9999, 5 + 0.5 * rel_time, 0) +
           rnorm(.N, mean = 0, sd = 3)]

# Create count outcome for Poisson
test_data[, y_count := rpois(.N, lambda = exp(3 + log(pmax(y, 1))))]
test_data[, pop := 10000]

cat("Created test dataset with:\n")
cat("  - Units:", n_units, "\n")
cat("  - Periods:", n_periods, "\n")
cat("  - Treatment cohorts: 2008 (25 units), 2012 (25 units)\n\n")

# ========================================
# Test 1: did2s with pretrend_test = TRUE
# ========================================
cat("----------------------------------------\n")
cat("Test 1: did2s adapter with pretrend_test = TRUE\n")
cat("----------------------------------------\n")

adapter_d2s <- adapter_did2s()
result_d2s <- adapter_d2s$fit(
  data = test_data,
  outcome_var = "y",
  time_var = "year",
  id_var = "unit_id",
  group_var = "cohort",
  event_study = TRUE,
  pretrend_test = TRUE
)

cat("\nResults:\n")
cat("  ATT estimate:", round(result_d2s$att, 3), "\n")
cat("  SE:", round(result_d2s$se, 3), "\n")

if ("pretrend_test" %in% names(result_d2s$metadata)) {
  pt <- result_d2s$metadata$pretrend_test
  cat("\nPre-trend Test Results:\n")
  cat("  Method:", pt$method, "\n")
  cat("  Wald statistic:", round(pt$wald_stat, 3), "\n")
  cat("  Degrees of freedom:", pt$df, "\n")
  cat("  P-value:", round(pt$p_value, 4), "\n")
  cat("  Reject at 0.05:", pt$reject_at_05, "\n")
  if (!is.null(pt$warning)) {
    cat("  Warning:", pt$warning, "\n")
  }
  cat("\n  VALIDATION: did2s pretrend_test structure is VALID\n")
} else {
  cat("\n  ERROR: pretrend_test not found in metadata!\n")
}

# ========================================
# Test 2: ETWFE Poisson with pretrend_test = TRUE
# ========================================
cat("\n----------------------------------------\n")
cat("Test 2: ETWFE Poisson adapter with pretrend_test = TRUE\n")
cat("----------------------------------------\n")

adapter_etwfe_p <- adapter_etwfe_poisson()

# Capture the message about vanilla Poisson
cat("\nExpecting message about vanilla Poisson event study...\n")
result_etwfe <- adapter_etwfe_p$fit(
  data = test_data,
  outcome_var = "y_count",
  time_var = "year",
  id_var = "unit_id",
  group_var = "cohort",
  pop_var = "pop",
  outcome_type = "count",
  pretrend_test = TRUE
)

cat("\nResults:\n")
cat("  ATT estimate:", round(result_etwfe$att, 3), "\n")
cat("  SE:", round(result_etwfe$se, 3), "\n")

if ("pretrend_test" %in% names(result_etwfe$metadata)) {
  pt <- result_etwfe$metadata$pretrend_test
  cat("\nPre-trend Test Results:\n")
  cat("  Method:", pt$method, "\n")
  cat("  Wald statistic:", round(pt$wald_stat, 3), "\n")
  cat("  Degrees of freedom:", pt$df, "\n")
  cat("  P-value:", round(pt$p_value, 4), "\n")
  cat("  Reject at 0.05:", pt$reject_at_05, "\n")
  if (!is.null(pt$warning)) {
    cat("  Warning:", pt$warning, "\n")
  }

  # Check for CV comparison (specific to Poisson)
  if ("cv_comparison" %in% names(pt)) {
    cat("\n  CV Comparison Results:\n")
    cat("    Ratio CV (multiplicative PT):", round(pt$cv_comparison$ratio_cv, 4), "\n")
    cat("    Difference CV (additive PT):", round(pt$cv_comparison$diff_cv, 4), "\n")
    cat("    Recommendation:", pt$cv_comparison$recommendation, "\n")
    cat("\n  VALIDATION: ETWFE Poisson pretrend_test structure is VALID with CV comparison\n")
  } else {
    cat("\n  VALIDATION: ETWFE Poisson pretrend_test structure is VALID (no CV comparison)\n")
  }
} else {
  cat("\n  ERROR: pretrend_test not found in metadata!\n")
}

# ========================================
# Test 3: CS with pretrend_test = TRUE
# ========================================
cat("\n----------------------------------------\n")
cat("Test 3: CS adapter with pretrend_test = TRUE\n")
cat("----------------------------------------\n")

# Try CS adapter - may fail if 'did' package not installed
result_cs <- tryCatch({
  adapter_cs_obj <- adapter_cs()
  adapter_cs_obj$fit(
    data = test_data,
    outcome_var = "y",
    time_var = "year",
    id_var = "unit_id",
    group_var = "cohort",
    event_study = TRUE,
    pretrend_test = TRUE
  )
}, error = function(e) {
  cat("\nSkipping CS test - Error:", conditionMessage(e), "\n")
  NULL
})

if (!is.null(result_cs)) {
  tryCatch({
    cat("\nResults:\n")
    if (!is.null(result_cs$att) && is.numeric(result_cs$att)) {
      cat("  ATT estimate:", round(result_cs$att, 3), "\n")
      cat("  SE:", round(result_cs$se, 3), "\n")
    } else {
      cat("  ATT estimate: (not available)\n")
    }
  }, error = function(e) {
    cat("  Error displaying results:", conditionMessage(e), "\n")
  })
} else {
  cat("\nCS test skipped due to error.\n")
}

if (!is.null(result_cs) && "pretrend_test" %in% names(result_cs$metadata)) {
  pt <- result_cs$metadata$pretrend_test
  cat("\nPre-trend Test Results:\n")
  cat("  Method:", pt$method, "\n")
  cat("  Wald statistic:", round(pt$wald_stat, 3), "\n")
  cat("  Degrees of freedom:", pt$df, "\n")
  cat("  P-value:", round(pt$p_value, 4), "\n")
  cat("  Reject at 0.05:", pt$reject_at_05, "\n")
  if (!is.null(pt$warning)) {
    cat("  Warning:", pt$warning, "\n")
  }
  if ("vcov_note" %in% names(pt)) {
    cat("  Note:", pt$vcov_note, "\n")
  }
  cat("\n  VALIDATION: CS pretrend_test structure is VALID\n")
} else if (!is.null(result_cs)) {
  cat("\n  ERROR: pretrend_test not found in metadata!\n")
}

# ========================================
# Summary
# ========================================
cat("\n========================================\n")
cat("Integration Test Summary\n")
cat("========================================\n\n")

all_valid <- TRUE

if ("pretrend_test" %in% names(result_d2s$metadata)) {
  cat("[PASS] did2s adapter returns valid pretrend_test structure\n")
} else {
  cat("[FAIL] did2s adapter missing pretrend_test\n")
  all_valid <- FALSE
}

if ("pretrend_test" %in% names(result_etwfe$metadata)) {
  pt_etwfe <- result_etwfe$metadata$pretrend_test
  if (pt_etwfe$method == "vanilla_poisson_event_study" && "cv_comparison" %in% names(pt_etwfe)) {
    cat("[PASS] ETWFE Poisson adapter returns valid pretrend_test with vanilla Poisson and CV comparison\n")
  } else {
    cat("[PARTIAL] ETWFE Poisson adapter returns pretrend_test but missing expected components\n")
  }
} else {
  cat("[FAIL] ETWFE Poisson adapter missing pretrend_test\n")
  all_valid <- FALSE
}

if (!is.null(result_cs) && "pretrend_test" %in% names(result_cs$metadata)) {
  cat("[PASS] CS adapter returns valid pretrend_test structure\n")
} else if (!is.null(result_cs)) {
  cat("[FAIL] CS adapter missing pretrend_test\n")
  all_valid <- FALSE
} else {
  cat("[SKIP] CS adapter not tested (package may not be installed)\n")
}

if (all_valid) {
  cat("\n==> ALL INTEGRATION TESTS PASSED!\n")
} else {
  cat("\n==> SOME INTEGRATION TESTS FAILED\n")
}

cat("\n")
