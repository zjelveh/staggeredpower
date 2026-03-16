#!/usr/bin/env Rscript
# Debug why controls are failing

library(staggeredpower)
library(data.table)
library(doParallel)

# Set up minimal parallel backend
cl <- makeCluster(4)
registerDoParallel(cl)

# Load data
setwd('/home/zjelveh/Dropbox/research/base_rate_fallacy/repo/base_rate_fallacy')
source('code/paper_code/estimation/create_datasets.R')
datasets <- create_datasets()
dat <- datasets$state

cat("=== Testing Controls Bug ===\n\n")

# Test 1: No controls (should work)
cat("Test 1: Running without controls...\n")
result_no_controls <- run_power_analysis(
  data_clean = dat,
  unit_var = 'state_fips',
  group_var = 'year_passed',
  time_var = 'year',
  rel_pass_var = 'rel_pass',
  treat_ind_var = 'law_pass',
  outcome = "y_shr_female_18",
  min_year = 1995,
  max_year = 2019,
  pta_type = "cs",
  percent_effect = 1.1,
  controls = NULL,
  models_to_run = "cs",
  n_sims = 3,
  use_v2 = TRUE
)

cat("Result without controls:\n")
if (!is.null(result_no_controls)) {
  cat("  - final_power rows:", nrow(result_no_controls$final_power), "\n")
  cat("  - controls column:", unique(result_no_controls$final_power$controls), "\n")
  cat("  - mean ATT:", mean(result_no_controls$final_power$att), "\n\n")
} else {
  cat("  - RESULT IS NULL!\n\n")
}

# Test 2: With unemp_rate control (currently failing)
cat("Test 2: Running WITH unemp_rate control...\n")
result_with_controls <- run_power_analysis(
  data_clean = dat,
  unit_var = 'state_fips',
  group_var = 'year_passed',
  time_var = 'year',
  rel_pass_var = 'rel_pass',
  treat_ind_var = 'law_pass',
  outcome = "y_shr_female_18",
  min_year = 1995,
  max_year = 2019,
  pta_type = "cs",
  percent_effect = 1.1,
  controls = c("unemp_rate"),
  models_to_run = "cs",
  n_sims = 3,
  use_v2 = TRUE
)

cat("Result WITH controls:\n")
if (!is.null(result_with_controls)) {
  cat("  - final_power rows:", nrow(result_with_controls$final_power), "\n")
  cat("  - controls column:", unique(result_with_controls$final_power$controls), "\n")
  cat("  - mean ATT:", mean(result_with_controls$final_power$att), "\n\n")
} else {
  cat("  - RESULT IS NULL!\n\n")
}

stopCluster(cl)

cat("=== Debug complete ===\n")
