# Large-Scale Power Analysis Example
#
# This example demonstrates using run_power_grid() to perform large-scale power
# analysis across multiple outcomes, effect sizes, and model specifications.
# It uses a YAML configuration file to control the grid search parameters.
#
# IMPORTANT: You MUST set up a parallel backend BEFORE running this script!
# See the "Required Parallel Setup" section below.
#
# Key features:
# - Process multiple outcomes in a single run_power_grid() call
# - Configure year ranges (e.g., 1995-2019) instead of hardcoded values
# - Use config-driven grid search for reproducibility
# - Store results in SQLite database for later analysis

library(staggeredpower)
library(data.table)
library(yaml)
library(RSQLite)
library(doParallel)

# =============================================================================
# 1. REQUIRED: Set Up Parallel Backend
# =============================================================================

# CRITICAL: run_power_analysis() uses %dopar% for Monte Carlo simulations
# You MUST register a parallel backend before running the analysis
cl <- makeCluster(detectCores() - 1)  # Adjust cores based on your hardware
registerDoParallel(cl)
cat(sprintf("Registered parallel backend with %d cores for Monte Carlo simulations\n\n",
            detectCores() - 1))

# =============================================================================
# 2. Load Configuration
# =============================================================================

# The config file defines the grid of parameters to test
config <- read_yaml('pwr_config.yaml')
config_to_run <- config$full_config

# Config structure:
# - transform_outcomes: outcome transformations (NULL, "log")
# - analysis_level: unit of analysis ("state_fips", "county_name")
# - data_source: data sources to analyze ("nibrs", "shr")
# - use_controls: control variable combinations
# - models_to_run: which estimators to use ("cs", "imputation")
# - n_sims: number of Monte Carlo simulations
# - sim_effects: range of effect sizes to test
# - enforce_pta: PTA enforcement methods and control sets

# =============================================================================
# 2. Load Your Data
# =============================================================================

# PLACEHOLDER: Replace with your data loading code
# Example:
#   source('path/to/create_datasets.R')
#   datasets <- create_datasets()
#   data <- datasets$state  # or datasets$county

# Your data should be a balanced panel with columns:
# - Unit identifier (e.g., state_fips, county_name)
# - Time variable (e.g., year)
# - Treatment cohort (e.g., year_passed)
# - Treatment indicator (e.g., law_pass)
# - Relative time to treatment (e.g., rel_pass)
# - Outcome variables
# - Control variables (optional)

# For this example, assume we have:
# data <- your_data_here

# =============================================================================
# 3. Helper Function: Extract Outcomes by Data Source
# =============================================================================

#' Get outcome variable names from dataset based on data source and sex
#'
#' @param data A data.table containing the analysis data
#' @param data_source Character. Either "nibrs" or "shr"
#' @param sex Character. Either "female" or "male" (default: "female")
#' @return Character vector of outcome column names
get_outcomes <- function(data, data_source, sex = "female") {
  if (data_source == "nibrs") {
    # NIBRS outcomes: aggravated share and rates for 18+ population
    outcomes <- names(data)[grepl('y_nibrs', names(data))]
    outcomes <- outcomes[grepl('aggshare|18', outcomes)]
  } else if (data_source == "shr") {
    # SHR outcomes: homicide rates
    outcomes <- names(data)[grepl('y_shr', names(data))]
  } else {
    stop("data_source must be 'nibrs' or 'shr'")
  }

  # Filter by sex
  outcomes <- outcomes[grepl(sex, outcomes)]

  return(sort(outcomes))
}

# =============================================================================
# 4. Set Up Database Connection
# =============================================================================

# Create or connect to SQLite database for storing results
db <- dbConnect(SQLite(), "power_analysis_results.sqlite")

# Create tables if they don't exist
if (!dbExistsTable(db, "final_power")) {
  # This will be created automatically when we write the first results
  message("Database tables will be created on first write")
}

# =============================================================================
# 5. Run Grid Search
# =============================================================================

# Iterate through major configuration parameters
# Note: run_power_grid() handles the inner loops (effect sizes, controls, etc.)

for (data_source in config_to_run$data_source) {
  for (analysis_level in config_to_run$analysis_level) {

    cat(sprintf("\n=== Processing: %s level, %s data ===\n",
                analysis_level, data_source))

    # Select appropriate dataset
    # PLACEHOLDER: Replace with your data selection logic
    # dat <- if (analysis_level == 'state_fips') datasets$state else datasets$county

    # Extract outcome variables for this data source
    # outcomes <- get_outcomes(dat, data_source, sex = "female")

    # EXAMPLE: For demonstration purposes, assume we have these outcomes
    # outcomes <- c("y_nibrs_female_agg_18", "y_nibrs_female_aggshare")

    cat(sprintf("Found %d outcomes: %s\n",
                length(outcomes),
                paste(head(outcomes, 3), collapse = ", ")))

    # Run power grid for ALL outcomes at once
    # This is the key improvement over the old approach!
    results <- run_power_grid(
      data_clean = dat,

      # Panel structure
      unit_var = analysis_level,
      group_var = "year_passed",
      time_var = "year",
      rel_pass_var = "rel_pass",
      treat_ind_var = "law_pass",

      # MULTIPLE OUTCOMES - processed in single call
      outcome = outcomes,

      # Year range (configurable, not hardcoded!)
      min_year = 1995,
      max_year = 2019,

      # Grid parameters from config
      pta_type = names(config_to_run$enforce_pta),

      # Build enforce_type list from config
      # enforce_type = list(NULL, c("unemp_rate"), c("unemp_rate", "all_crimes_rate")),

      # Control variable combinations to test
      controls = config_to_run$use_controls,

      # Effect sizes to test
      percent_effect = seq(
        config_to_run$sim_effects$min,
        config_to_run$sim_effects$max,
        config_to_run$sim_effects$skip
      ),

      # Estimators to use
      models_to_run = config_to_run$models_to_run,

      # Monte Carlo simulations
      n_sims = config_to_run$n_sims,

      # Run grid sequentially (recommended)
      # Parallelization happens at Monte Carlo sim level (via doParallel backend above)
      # Setting parallel=TRUE would require n_specs × n_cores total cores!
      parallel = FALSE
    )

    # ==========================================================================
    # 6. Save Results to Database
    # ==========================================================================

    # Add metadata columns for tracking
    results$final_power[, `:=`(
      data_source = data_source,
      analysis_level = analysis_level,
      run_date = Sys.Date()
    )]

    results$power_summary[, `:=`(
      data_source = data_source,
      analysis_level = analysis_level,
      run_date = Sys.Date()
    )]

    # Write to database with retry logic for database locks
    max_attempts <- 5
    attempt <- 1

    while (attempt <= max_attempts) {
      tryCatch({
        # Write main results
        dbWriteTable(db, "final_power", results$final_power, append = TRUE)

        # Write power summary
        dbWriteTable(db, "power_summary", results$power_summary, append = TRUE)

        # Write specifications grid
        if (!is.null(results$specifications)) {
          results$specifications[, `:=`(
            data_source = data_source,
            analysis_level = analysis_level
          )]
          dbWriteTable(db, "specifications", results$specifications, append = TRUE)
        }

        cat(sprintf("Successfully saved %d result rows to database\n",
                    nrow(results$final_power)))
        break

      }, error = function(e) {
        message(sprintf("Attempt %d failed: %s", attempt, e$message))
        attempt <- attempt + 1

        if (attempt > max_attempts) {
          stop("Failed to write to database after ", max_attempts, " attempts")
        }

        # Exponential backoff
        Sys.sleep(runif(1, 0.1, 0.5) * attempt)
      })
    }
  }
}

# =============================================================================
# 7. Clean Up
# =============================================================================

dbDisconnect(db)

# Stop parallel cluster
stopCluster(cl)
cat("\nClosed parallel cluster\n")

cat("\n=== Analysis complete! ===\n")

# =============================================================================
# 8. Query Results (Example)
# =============================================================================

# You can query the results later like this:
# db <- dbConnect(SQLite(), "power_analysis_results.sqlite")
#
# # Get power summary for a specific outcome
# power_by_outcome <- dbGetQuery(db, "
#   SELECT outcome, model, percent_effect, AVG(power) as avg_power
#   FROM power_summary
#   WHERE data_source = 'nibrs'
#   GROUP BY outcome, model, percent_effect
#   ORDER BY outcome, percent_effect
# ")
#
# # Plot power curves
# library(ggplot2)
# ggplot(power_by_outcome, aes(x = percent_effect, y = avg_power, color = model)) +
#   geom_line() +
#   facet_wrap(~outcome) +
#   geom_hline(yintercept = 0.80, linetype = "dashed") +
#   theme_minimal()
#
# dbDisconnect(db)
