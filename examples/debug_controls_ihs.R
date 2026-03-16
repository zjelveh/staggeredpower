#!/usr/bin/env Rscript
# Test if IHS + controls combination is the issue

library(staggeredpower)
library(data.table)
library(doParallel)

cl <- makeCluster(4)
registerDoParallel(cl)

setwd('/home/zjelveh/Dropbox/research/base_rate_fallacy/repo/base_rate_fallacy')
source('code/paper_code/estimation/create_datasets.R')
datasets <- create_datasets()
dat <- datasets$state

cat("=== Testing IHS + Controls Combination ===\n\n")

# Test 1: IHS WITHOUT controls (from our earlier logs, this worked)
cat("Test 1: IHS WITHOUT controls...\n")
result_ihs_no_controls <- run_power_analysis(
  data_clean = dat,
  unit_var = 'state_fips',
  group_var = 'year_passed',
  time_var = 'year',
  rel_pass_var = 'rel_pass',
  treat_ind_var = 'law_pass',
  outcome = "y_shr_female_18",
  transform_outcome = "ihs",
  min_year = 1995,
  max_year = 2019,
  pta_type = "cs",
  percent_effect = 1.1,
  controls = NULL,
  models_to_run = "cs",
  n_sims = 3,
  use_v2 = TRUE
)

cat("Result:\n")
if (!is.null(result_ihs_no_controls)) {
  cat("  - Rows:", nrow(result_ihs_no_controls$final_power), "\n")
  cat("  - Mean ATT:", mean(result_ihs_no_controls$final_power$att), "\n\n")
} else {
  cat("  - RESULT IS NULL!\n\n")
}

# Test 2: IHS WITH controls (this might be the failing case)
cat("Test 2: IHS WITH unemp_rate controls...\n")
result_ihs_with_controls <- run_power_analysis(
  data_clean = dat,
  unit_var = 'state_fips',
  group_var = 'year_passed',
  time_var = 'year',
  rel_pass_var = 'rel_pass',
  treat_ind_var = 'law_pass',
  outcome = "y_shr_female_18",
  transform_outcome = "ihs",
  min_year = 1995,
  max_year = 2019,
  pta_type = "cs",
  percent_effect = 1.1,
  controls = c("unemp_rate"),
  models_to_run = "cs",
  n_sims = 3,
  use_v2 = TRUE
)

cat("Result:\n")
if (!is.null(result_ihs_with_controls)) {
  cat("  - Rows:", nrow(result_ihs_with_controls$final_power), "\n")
  cat("  - Mean ATT:", mean(result_ihs_with_controls$final_power$att), "\n\n")
} else {
  cat("  - RESULT IS NULL!\n\n")
}

stopCluster(cl)

cat("=== Debug complete ===\n")
