#!/usr/bin/env Rscript
# Power Analysis V3 - Complete specification with controls and transformations
#
# This creates a comprehensive database including:
# - Both untransformed and IHS-transformed outcomes
# - SHR and NIBRS outcomes
# - 5 control specifications (from no controls to full spec)
# - 100 Monte Carlo simulations per specification
# - Both CS and imputation estimators

library(staggeredpower)
library(data.table)
library(RSQLite)
library(foreach)
library(doParallel)

cat("=== Power Analysis V3: Full Specification ===\n\n")

# CRITICAL: Set up parallel backend for Monte Carlo simulations
cl <- makeCluster(50)
registerDoParallel(cl)
cat("Registered parallel backend with 50 cores for Monte Carlo simulations\n\n")

# Load strangulation data
setwd('/home/zjelveh/Dropbox/research/base_rate_fallacy/repo/base_rate_fallacy')
source('code/paper_code/estimation/create_datasets.R')
datasets <- create_datasets()
dat <- datasets$state

cat(sprintf("Loaded data: %d rows\n\n", nrow(dat)))

# Get outcome variables
nibrs_outcomes <- names(dat)[grepl('y_nibrs', names(dat))]
nibrs_outcomes <- nibrs_outcomes[grepl('female', nibrs_outcomes)]
nibrs_outcomes <- nibrs_outcomes[grepl('aggshare|18', nibrs_outcomes)]

shr_outcomes <- names(dat)[grepl('y_shr', names(dat))]
shr_outcomes <- shr_outcomes[grepl('female', shr_outcomes)]

outcomes <- sort(c(nibrs_outcomes, shr_outcomes))

cat(sprintf("Analyzing %d outcomes:\n", length(outcomes)))
cat(paste("  -", outcomes, collapse = "\n"), "\n\n")

# Database setup
db_file <- "/home/zjelveh/Dropbox/research/base_rate_fallacy/repo/staggeredpower/examples/power_analysis_results_V3.sqlite"
db <- dbConnect(SQLite(), db_file)

cat(sprintf("Database: %s\n\n", db_file))

# Run power analysis
cat("=== Starting Power Grid Analysis ===\n")
cat("This will take several hours...\n\n")

cat("Specification details:\n")
cat("  - Outcomes: 7 (4 NIBRS + 3 SHR)\n")
cat("  - Transformations: 2 (untransformed, IHS)\n")
cat("  - PTA types: 2 (CS, imputation)\n")
cat("  - Effect sizes: 11 (0.5 to 1.5 by 0.1)\n")
cat("  - Control specifications: 5\n")
cat("  - Models: 2 (CS, imputation)\n")
cat("  - Simulations per spec: 100\n")
cat("  - Total specifications: 7 × 2 × 2 × 11 × 5 × 2 = 3,080\n")
cat("  - Total simulation runs: 3,080 × 100 = 308,000\n\n")

start_time <- Sys.time()

results <- run_power_grid(
  data_clean = dat,
  unit_var = 'state_fips',
  group_var = 'year_passed',
  time_var = 'year',
  rel_pass_var = 'rel_pass',
  treat_ind_var = 'law_pass',

  # All outcomes
  outcome = outcomes,

  # Year filtering
  min_year = 1995,
  max_year = 2019,

  # Both PTA methods
  pta_type = c("cs", "imputation"),

  # Effect sizes from -50% to +50%
  percent_effect = seq(0.5, 1.5, 0.1),

  # BOTH transformations: untransformed and IHS
  transform_outcome = list(NULL, "ihs"),

  # 5 control specifications
  controls = list(
    # Spec 1: No controls (baseline)
    NULL,

    # Spec 2: Economic only
    c("unemp_rate"),

    # Spec 3: Economic + Crime environment
    c("unemp_rate", "index_violent_crimes_rate"),

    # Spec 4: Economic + Crime + Concurrent DV policies
    c("unemp_rate", "index_violent_crimes_rate", "is_disc", "is_mand"),

    # Spec 5: Full specification
    c("unemp_rate", "index_violent_crimes_rate", "is_disc", "is_mand",
      "population_overall")
  ),

  # Both models
  models_to_run = c("cs", "imputation"),

  # 100 simulations (publication quality)
  n_sims = 100,

  # Run grid sequentially (parallel happens at Monte Carlo level)
  parallel = FALSE
)

end_time <- Sys.time()
duration <- difftime(end_time, start_time, units = "hours")

cat("\n=== Power analysis complete! ===\n")
cat(sprintf("Total runtime: %.2f hours\n\n", as.numeric(duration)))

# Add metadata
results$final_power[, `:=`(
  data_source = ifelse(grepl('y_nibrs', outcome), "nibrs", "shr"),
  analysis_level = "state_fips",
  run_date = Sys.Date(),
  version = "V3"
)]

results$power_summary[, `:=`(
  data_source = ifelse(grepl('y_nibrs', outcome), "nibrs", "shr"),
  analysis_level = "state_fips",
  run_date = Sys.Date(),
  version = "V3"
)]

# Save to database
cat("Writing results to database...\n")

dbWriteTable(db, "final_power", results$final_power, overwrite = TRUE)
cat(sprintf("  ✓ Wrote %d rows to final_power table\n", nrow(results$final_power)))

dbWriteTable(db, "power_summary", results$power_summary, overwrite = TRUE)
cat(sprintf("  ✓ Wrote %d rows to power_summary table\n", nrow(results$power_summary)))

# Summary statistics
cat("\n=== Database Summary ===\n")

summary_stats <- dbGetQuery(db, "
  SELECT
    outcome_transformed,
    controls,
    COUNT(*) as n_rows,
    COUNT(DISTINCT outcome) as n_outcomes,
    COUNT(DISTINCT percent_effect) as n_effects
  FROM final_power
  GROUP BY outcome_transformed, controls
  ORDER BY outcome_transformed, controls
")

cat("\nRows by transformation and controls:\n")
print(summary_stats)

cat("\n\nControl specifications breakdown:\n")
controls_breakdown <- dbGetQuery(db, "
  SELECT
    controls,
    COUNT(DISTINCT outcome) as outcomes,
    COUNT(*) as total_rows
  FROM final_power
  GROUP BY controls
")
print(controls_breakdown)

cat("\n\nTransformation breakdown:\n")
transform_breakdown <- dbGetQuery(db, "
  SELECT
    outcome_transformed,
    COUNT(DISTINCT outcome) as outcomes,
    COUNT(*) as total_rows
  FROM final_power
  GROUP BY outcome_transformed
")
print(transform_breakdown)

# Close connections
dbDisconnect(db)
stopCluster(cl)

cat("\n=== SUCCESS ===\n")
cat(sprintf("V3 database created: %s\n", db_file))
cat("\nNext steps:\n")
cat("  1. Run analyze_results_v3.R to generate reports and plots\n")
cat("  2. Compare power across control specifications\n")
cat("  3. Examine transformation effects\n")
