#!/usr/bin/env Rscript
# Test run_power_grid with controls

library(staggeredpower)
library(data.table)
library(doParallel)

cl <- makeCluster(4)
registerDoParallel(cl)

setwd('/home/zjelveh/Dropbox/research/base_rate_fallacy/repo/base_rate_fallacy')
source('code/paper_code/estimation/create_datasets.R')
datasets <- create_datasets()
dat <- datasets$state

cat("=== Testing run_power_grid with Controls ===\n\n")

# Minimal grid with just 2 effect sizes and 2 control settings
result <- run_power_grid(
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
  percent_effect = c(1.0, 1.1),  # Just 2 effect sizes
  transform_outcome = "ihs",

  # THE KEY TEST: Both NULL and unemp_rate
  controls = list(NULL, c("unemp_rate")),

  models_to_run = "cs",
  n_sims = 3,
  parallel = FALSE  # Sequential for easier debugging
)

cat("\n=== Results ===\n")
cat("Total rows in final_power:", nrow(result$final_power), "\n")
cat("Expected rows: 2 effect sizes × 2 controls × 3 sims = 12\n\n")

cat("Breakdown by controls:\n")
controls_table <- result$final_power[, .N, by = controls]
print(controls_table)

cat("\nDetailed power by controls and effect:\n")
power_table <- result$final_power[, .(
  power = mean(abs(att/se) > 1.96),
  mean_att = mean(att),
  n_sims = .N
), by = .(controls, percent_effect)]
print(power_table)

stopCluster(cl)

cat("\n=== Done ===\n")
