#!/usr/bin/env Rscript
# Compare results from original API vs adapter pattern
# Runs same power analysis with use_v2=FALSE and use_v2=TRUE
# Saves results to compare_results.sqlite

library(staggeredpower)
library(data.table)
library(RSQLite)
library(foreach)
library(doParallel)

cat("=== Comparing Original API vs Adapter Pattern ===\n\n")

# Set up parallel backend
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
cat(sprintf("Registered parallel backend with %d cores\n\n", detectCores() - 1))

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

# Use subset for quick comparison
outcomes <- nibrs_outcomes[1:2]  # Just first 2 outcomes

cat(sprintf("Testing %d outcomes: %s\n\n", length(outcomes), paste(outcomes, collapse = ", ")))

# Database for results
db_file <- "examples/compare_results.sqlite"
db <- dbConnect(SQLite(), db_file)

cat(sprintf("Results will be saved to: %s\n\n", db_file))

# Fixed seed for reproducibility
set.seed(42)

# =============================================================================
# Run 1: Original API (use_v2=FALSE)
# =============================================================================

cat("=== RUN 1: Original API (use_v2=FALSE) ===\n")
cat("This uses the legacy estimate_models() function\n\n")

results_v1 <- run_power_grid(
  data_clean = dat,
  unit_var = 'state_fips',
  group_var = 'year_passed',
  time_var = 'year',
  rel_pass_var = 'rel_pass',
  treat_ind_var = 'law_pass',
  outcome = outcomes,
  min_year = 1995,
  max_year = 2019,
  pta_type = "cs",
  percent_effect = c(0.9, 1.0, 1.1),  # Small set for quick test
  controls = list(NULL),
  models_to_run = "cs",
  n_sims = 10,  # Moderate number of sims
  parallel = FALSE,
  use_v2 = FALSE  # ORIGINAL API
)

cat(sprintf("✓ Original API completed: %d results\n\n", nrow(results_v1$final_power)))

# Reset seed
set.seed(42)

# =============================================================================
# Run 2: Adapter Pattern (use_v2=TRUE)
# =============================================================================

cat("=== RUN 2: Adapter Pattern (use_v2=TRUE) ===\n")
cat("This uses the new estimate_models_v2() with adapters\n\n")

results_v2 <- run_power_grid(
  data_clean = dat,
  unit_var = 'state_fips',
  group_var = 'year_passed',
  time_var = 'year',
  rel_pass_var = 'rel_pass',
  treat_ind_var = 'law_pass',
  outcome = outcomes,
  min_year = 1995,
  max_year = 2019,
  pta_type = "cs",
  percent_effect = c(0.9, 1.0, 1.1),
  controls = list(NULL),
  models_to_run = "cs",
  n_sims = 10,
  parallel = FALSE,
  use_v2 = TRUE  # ADAPTER PATTERN
)

cat(sprintf("✓ Adapter pattern completed: %d results\n\n", nrow(results_v2$final_power)))

# =============================================================================
# Compare Results
# =============================================================================

cat("=== COMPARISON ===\n\n")

# Add version tags
results_v1$final_power[, api_version := "v1_original"]
results_v2$final_power[, api_version := "v2_adapter"]

# Combine
all_results <- rbind(results_v1$final_power, results_v2$final_power)

# Save to database
dbWriteTable(db, "comparison_results", all_results, overwrite = TRUE)
cat(sprintf("✓ Saved %d total results to database\n", nrow(all_results)))

# Compare ATT estimates
comparison <- merge(
  results_v1$final_power[, .(outcome, percent_effect, sim, att_v1 = att, se_v1 = se)],
  results_v2$final_power[, .(outcome, percent_effect, sim, att_v2 = att, se_v2 = se)],
  by = c("outcome", "percent_effect", "sim")
)

comparison[, `:=`(
  att_diff = att_v2 - att_v1,
  att_pct_diff = 100 * (att_v2 - att_v1) / abs(att_v1),
  se_diff = se_v2 - se_v1,
  se_pct_diff = 100 * (se_v2 - se_v1) / abs(se_v1)
)]

cat("\nATT Comparison Summary:\n")
cat(sprintf("  Mean absolute ATT difference: %.6f\n", mean(abs(comparison$att_diff))))
cat(sprintf("  Max absolute ATT difference: %.6f\n", max(abs(comparison$att_diff))))
cat(sprintf("  Mean percent ATT difference: %.3f%%\n", mean(abs(comparison$att_pct_diff))))
cat(sprintf("  Max percent ATT difference: %.3f%%\n", max(abs(comparison$att_pct_diff))))

cat("\nSE Comparison Summary:\n")
cat(sprintf("  Mean absolute SE difference: %.6f\n", mean(abs(comparison$se_diff))))
cat(sprintf("  Max absolute SE difference: %.6f\n", max(abs(comparison$se_diff))))
cat(sprintf("  Mean percent SE difference: %.3f%%\n", mean(abs(comparison$se_pct_diff))))
cat(sprintf("  Max percent SE difference: %.3f%%\n", max(abs(comparison$se_pct_diff))))

# Save comparison table
dbWriteTable(db, "detailed_comparison", comparison, overwrite = TRUE)

# Check if results are essentially identical (allowing for tiny numerical differences)
tolerance <- 1e-10
att_match <- all(abs(comparison$att_diff) < tolerance)
se_match <- all(abs(comparison$se_diff) < tolerance)

cat("\n=== VERDICT ===\n")
if (att_match && se_match) {
  cat("✓ PERFECT MATCH: Results are numerically identical!\n")
  cat("  The adapter pattern produces exactly the same estimates.\n")
} else if (mean(abs(comparison$att_pct_diff)) < 0.01) {
  cat("✓ VERY CLOSE: Results differ by less than 0.01% on average\n")
  cat("  Minor numerical differences due to floating point precision.\n")
} else {
  cat("⚠ DIFFERENCES DETECTED\n")
  cat("  There are meaningful differences between v1 and v2.\n")
  cat("  This may indicate an issue with the adapter implementation.\n")
}

dbDisconnect(db)
stopCluster(cl)

cat(sprintf("\n✓ Comparison complete! Results saved to: %s\n", db_file))
cat("\nYou can query the results with:\n")
cat("  db <- dbConnect(RSQLite::SQLite(), 'examples/compare_results.sqlite')\n")
cat("  dbGetQuery(db, 'SELECT * FROM detailed_comparison')\n")
