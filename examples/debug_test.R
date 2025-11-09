#!/usr/bin/env Rscript
# Minimal diagnostic test to understand the failure

library(staggeredpower)
library(data.table)

cat("=== Diagnostic Test ===\n\n")

# Load data
setwd('/home/zjelveh/Dropbox/research/base_rate_fallacy/repo/base_rate_fallacy')
source('code/paper_code/estimation/create_datasets.R')
datasets <- create_datasets()
dat <- datasets$state

# Test with just ONE outcome, ONE specification, sequential execution
cat("Testing single specification...\n")

results <- tryCatch({
  run_power_grid(
    data_clean = dat,
    unit_var = 'state_fips',
    group_var = 'year_passed',
    time_var = 'year',
    rel_pass_var = 'rel_pass',
    treat_ind_var = 'law_pass',

    # Just ONE outcome
    outcome = 'y_nibrs_female_agg_18',

    # Just ONE effect size
    percent_effect = 0.50,

    # ONE PTA type
    pta_type = 'cs',

    # NO controls
    controls = list(NULL),

    # ONE model
    models_to_run = c('cs'),

    # Few sims
    n_sims = 5,

    # Year range
    min_year = 1995,
    max_year = 2019,

    # Sequential (no parallel)
    parallel = FALSE
  )
}, error = function(e) {
  cat("\n!!! ERROR !!!\n")
  cat(sprintf("Error message: %s\n", e$message))
  cat(sprintf("Error call: %s\n", deparse(e$call)))
  print(traceback())
  return(NULL)
})

if (!is.null(results)) {
  cat("\n✓ SUCCESS!\n")
  cat(sprintf("final_power rows: %d\n", nrow(results$final_power)))
  cat(sprintf("power_summary rows: %d\n", nrow(results$power_summary)))
  cat(sprintf("Columns in final_power: %s\n", paste(names(results$final_power), collapse=", ")))
} else {
  cat("\n✗ FAILED - see error above\n")
}
