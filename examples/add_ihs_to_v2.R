#!/usr/bin/env Rscript
# Add IHS-transformed outcomes to existing V2 database
# This runs the same spec as test_strangulation.R but with transform_outcome="ihs"

library(staggeredpower)
library(data.table)
library(RSQLite)
library(foreach)
library(doParallel)

cat("=== Adding IHS-transformed outcomes to V2 database ===\n\n")

# CRITICAL: Set up parallel backend for run_power_analysis()
cl <- makeCluster(50)
registerDoParallel(cl)
cat("Registered parallel backend with 50 cores for Monte Carlo simulations\n\n")

# Load strangulation data
setwd('/home/zjelveh/Dropbox/research/base_rate_fallacy/repo/base_rate_fallacy')
source('code/paper_code/estimation/create_datasets.R')
datasets <- create_datasets()
dat <- datasets$state

cat(sprintf("Loaded data: %d rows\n", nrow(dat)))

# Get NIBRS female outcomes
nibrs_outcomes <- names(dat)[grepl('y_nibrs', names(dat))]
nibrs_outcomes <- nibrs_outcomes[grepl('female', nibrs_outcomes)]
nibrs_outcomes <- nibrs_outcomes[grepl('aggshare|18', nibrs_outcomes)]

# Get SHR female outcomes
shr_outcomes <- names(dat)[grepl('y_shr', names(dat))]
shr_outcomes <- shr_outcomes[grepl('female', shr_outcomes)]

# Combine all outcomes
outcomes <- sort(c(nibrs_outcomes, shr_outcomes))

cat(sprintf("Found %d outcomes: %s\n\n", length(outcomes), paste(outcomes, collapse = ", ")))

# Connect to EXISTING V2 database
db_file <- "/home/zjelveh/Dropbox/research/base_rate_fallacy/repo/staggeredpower/examples/power_analysis_results_V2.sqlite"
db <- dbConnect(SQLite(), db_file)

# Check current status
cat("Checking current database status...\n")
current_count <- dbGetQuery(db, "SELECT COUNT(*) as n FROM final_power")
cat(sprintf("Current rows in final_power: %d\n", current_count$n))

transforms_present <- dbGetQuery(db, "SELECT DISTINCT outcome_transformed FROM final_power")
cat("Current transformations in database:\n")
print(transforms_present)
cat("\n")

# Run power grid with IHS transformation
cat("Starting power analysis with IHS transformation...\n")
cat("This may take several minutes...\n\n")

results <- run_power_grid(
  data_clean = dat,
  unit_var = 'state_fips',
  group_var = 'year_passed',
  time_var = 'year',
  rel_pass_var = 'rel_pass',
  treat_ind_var = 'law_pass',

  # Same outcomes as original V2 spec
  outcome = outcomes,

  # Year filtering
  min_year = 1995,
  max_year = 2019,

  # Same PTA methods
  pta_type = c("cs", "imputation"),

  # Same effect sizes
  percent_effect = seq(0.5, 1.5, 0.1),

  # NEW: IHS transformation!
  transform_outcome = "ihs",

  # Same controls
  controls = list(NULL, c("unemp_rate")),

  # Same models
  models_to_run = c("cs", "imputation"),

  # Same number of sims
  n_sims = 50,

  # Run grid sequentially
  parallel = FALSE
)

cat("\n=== Power analysis complete! ===\n\n")

# Add metadata
results$final_power[, `:=`(
  data_source = ifelse(grepl('y_nibrs', outcome), "nibrs", "shr"),
  analysis_level = "state_fips",
  run_date = Sys.Date()
)]

results$power_summary[, `:=`(
  data_source = ifelse(grepl('y_nibrs', outcome), "nibrs", "shr"),
  analysis_level = "state_fips",
  run_date = Sys.Date()
)]

# APPEND to existing database (overwrite = FALSE)
cat("Appending IHS results to database...\n")

dbWriteTable(db, "final_power", results$final_power, append = TRUE)
cat(sprintf("  ✓ Added %d rows to final_power table\n", nrow(results$final_power)))

dbWriteTable(db, "power_summary", results$power_summary, append = TRUE)
cat(sprintf("  ✓ Added %d rows to power_summary table\n", nrow(results$power_summary)))

# Verify additions
cat("\n=== Verification ===\n")
new_count <- dbGetQuery(db, "SELECT COUNT(*) as n FROM final_power")
cat(sprintf("New total rows in final_power: %d (added %d)\n", 
            new_count$n, new_count$n - current_count$n))

transforms_after <- dbGetQuery(db, "SELECT outcome_transformed, COUNT(*) as n FROM final_power GROUP BY outcome_transformed")
cat("\nTransformations in database after addition:\n")
print(transforms_after)

# Summary by transformation
cat("\n=== Summary by Transformation ===\n")
summary_by_transform <- dbGetQuery(db, 
  "SELECT outcome_transformed, 
          COUNT(DISTINCT outcome) as n_outcomes,
          COUNT(*) as n_rows,
          AVG(att) as mean_att
   FROM final_power
   GROUP BY outcome_transformed")
print(summary_by_transform)

dbDisconnect(db)

# Clean up parallel backend
stopCluster(cl)
cat("\nClosed parallel cluster\n")

cat(sprintf("\n✓ Database updated: %s\n", db_file))
cat("\n=== SUCCESS ===\n")
cat("IHS-transformed outcomes have been added to the V2 database!\n")
cat("The database now contains both untransformed and IHS-transformed results.\n")
