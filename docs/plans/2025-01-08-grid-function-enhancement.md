# Grid Function Enhancement Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Extend `run_power_grid()` to handle multiple outcomes and configurable year ranges, migrate large-scale analysis to examples/

**Architecture:** Add optional year range parameters to filter data, extend grid function to loop over outcome vectors internally, create examples/ folder demonstrating real-world config-driven analysis

**Tech Stack:** R, data.table, foreach/doParallel, yaml, RSQLite

---

## Phase 1: Add Year Range Parameters

### Task 1: Add year parameters to run_power_analysis()

**Files:**
- Modify: `R/power_analysis.R:13-26` (function signature)
- Modify: `R/power_analysis.R:34` (year filtering)
- Modify: `R/power_analysis.R:71` (PTA violation check)

**Step 1: Add parameters to function signature**

In `R/power_analysis.R`, update the function signature (lines 13-26):

```r
run_power_analysis <- function(data_clean,
                               unit_var,
                               group_var,
                               time_var,
                               rel_pass_var,
                               treat_ind_var,
                               controls=NULL,
                               outcome,
                               transform_outcome=NULL,
                               pta_type,
                               enforce_type=NULL,
                               percent_effect,
                               models_to_run=c('cs', 'imputation', 'twfe'),
                               n_sims = 100,
                               min_year = NULL,
                               max_year = NULL) {
```

**Step 2: Replace hardcoded year filter at line 34**

Find this line:
```r
data_clean_full = data_clean[between(year, 1995, 2019)]
```

Replace with:
```r
# Filter by year range if specified
if (!is.null(min_year) && !is.null(max_year)) {
  data_clean_full = data_clean[get(time_var) >= min_year & get(time_var) <= max_year]
} else if (!is.null(min_year)) {
  data_clean_full = data_clean[get(time_var) >= min_year]
} else if (!is.null(max_year)) {
  data_clean_full = data_clean[get(time_var) <= max_year]
} else {
  data_clean_full = data_clean
}
```

**Step 3: Fix PTA violation check at line 71**

Find this line:
```r
pta_violations = copy(pta_enforced[bound_error == 1 | (na_error == 1 & get(time_var) < 2019)])
```

Replace with:
```r
# Only filter by max_year if specified
max_year_check <- if (!is.null(max_year)) max_year else max(pta_enforced[[time_var]])
pta_violations = copy(pta_enforced[bound_error == 1 | (na_error == 1 & get(time_var) < max_year_check)])
```

**Step 4: Update function documentation**

At the top of the function (around line 2-12), add parameter docs:

```r
#' @param min_year Numeric. Minimum year to include (optional, default NULL = no minimum)
#' @param max_year Numeric. Maximum year to include (optional, default NULL = no maximum)
```

**Step 5: Test the changes**

Create test file to verify:
```r
# Test in R console
library(staggeredpower)
library(data.table)

# Create minimal test data
test_data <- data.table(
  state_fips = rep(1:5, each = 30),
  year = rep(1990:2019, times = 5),
  year_passed = rep(c(2000, 2005, NA, 2010, 2015), each = 30),
  law_pass = 0,
  rel_pass = 0,
  outcome = rnorm(150)
)
test_data[year >= year_passed & !is.na(year_passed), law_pass := 1]
test_data[, rel_pass := year - year_passed]

# Test with year range
result <- run_power_analysis(
  data_clean = test_data,
  unit_var = "state_fips",
  group_var = "year_passed",
  time_var = "year",
  rel_pass_var = "rel_pass",
  treat_ind_var = "law_pass",
  outcome = "outcome",
  pta_type = "cs",
  percent_effect = 0.1,
  n_sims = 2,
  min_year = 1995,
  max_year = 2019
)

# Check data was filtered
print(paste("Rows in result:", nrow(result$final_power)))
```

**Step 6: Commit**

```bash
git add R/power_analysis.R
git commit -m "feat: add min_year/max_year parameters to run_power_analysis

- Add optional year range filtering parameters
- Replace hardcoded 1995-2019 range
- Update PTA violation check to use max_year
- Backward compatible (NULL = use all data)"
```

---

### Task 2: Add year parameters to estimate_models()

**Files:**
- Modify: `R/estimate_models.R:11-20` (function signature)
- Modify: `R/estimate_models.R:26` (year filtering)

**Step 1: Add parameters to function signature**

In `R/estimate_models.R`, update function signature (lines 11-20):

```r
estimate_models <- function(data,
                            id_var,
                            outcome_var,
                            time_var,
                            group_var,
                            controls,
                            models_to_run,
                            cluster_var='state',
                            treat_ind_var='law_pass',
                            event_study = FALSE,
                            min_year = NULL,
                            max_year = NULL) {
```

**Step 2: Replace hardcoded year filter at line 26**

Find this line:
```r
analysis_data <- data[between(year, 1995, 2019)]
```

Replace with:
```r
# Filter by year range if specified
if (!is.null(min_year) && !is.null(max_year)) {
  analysis_data <- data[get(time_var) >= min_year & get(time_var) <= max_year]
} else if (!is.null(min_year)) {
  analysis_data <- data[get(time_var) >= min_year]
} else if (!is.null(max_year)) {
  analysis_data <- data[get(time_var) <= max_year]
} else {
  analysis_data <- data
}
```

**Step 3: Update function documentation**

Add parameter docs at top (around line 2-10):

```r
#' @param min_year Numeric. Minimum year to include (optional)
#' @param max_year Numeric. Maximum year to include (optional)
```

**Step 4: Commit**

```bash
git add R/estimate_models.R
git commit -m "feat: add min_year/max_year parameters to estimate_models

- Add optional year range filtering
- Replace hardcoded year filter
- Backward compatible"
```

---

### Task 3: Update run_power_grid() to pass year parameters

**Files:**
- Modify: `R/run_power_grid.R` (function signature and internal calls)

**Step 1: Read current run_power_grid.R to understand structure**

```bash
cat R/run_power_grid.R | head -50
```

**Step 2: Add year parameters to run_power_grid signature**

Add `min_year = NULL, max_year = NULL` to the function parameters.

**Step 3: Pass year params to run_power_analysis() calls**

Find all calls to `run_power_analysis()` inside the grid loop and add:
```r
min_year = min_year,
max_year = max_year,
```

**Step 4: Update documentation**

Add to roxygen comments:
```r
#' @param min_year Numeric. Minimum year for analysis (optional)
#' @param max_year Numeric. Maximum year for analysis (optional)
```

**Step 5: Test the grid function**

```r
# Test grid with year range
grid_result <- run_power_grid(
  data_clean = test_data,
  unit_var = "state_fips",
  group_var = "year_passed",
  time_var = "year",
  rel_pass_var = "rel_pass",
  treat_ind_var = "law_pass",
  outcome = "outcome",
  pta_type = c("cs", "imputation"),
  percent_effect = c(0.05, 0.10),
  n_sims = 2,
  min_year = 1995,
  max_year = 2019
)

print(grid_result$power_summary)
```

**Step 6: Commit**

```bash
git add R/run_power_grid.R
git commit -m "feat: add year range parameters to run_power_grid

- Pass min_year/max_year through to run_power_analysis
- Update documentation"
```

---

## Phase 2: Extend run_power_grid() for Multiple Outcomes

### Task 4: Modify run_power_grid() to accept outcome vector

**Files:**
- Modify: `R/run_power_grid.R` (entire function logic)

**Step 1: Read the current run_power_grid.R implementation**

```bash
cat R/run_power_grid.R
```

Note the current structure - it likely runs `run_power_analysis()` once per specification combination.

**Step 2: Add outcome loop wrapper**

Modify the function to wrap the existing grid logic in an outcome loop. The structure should be:

```r
run_power_grid <- function(
  data_clean,
  unit_var,
  group_var,
  time_var,
  rel_pass_var,
  treat_ind_var,
  outcome,              # Can now be vector
  min_year = NULL,
  max_year = NULL,
  pta_type = "cs",
  enforce_type = NULL,
  controls = NULL,
  percent_effect = 0.10,
  models_to_run = c("cs", "imputation"),
  n_sims = 100,
  parallel = FALSE,
  n_cores = NULL
) {

  # Ensure outcome is a vector
  if (!is.character(outcome)) {
    stop("outcome must be a character vector of column names")
  }

  # Initialize results storage
  all_final_power <- list()
  all_power_summary <- list()
  all_specifications <- list()

  # Loop over outcomes
  for (outcome_var in outcome) {
    cat(sprintf("\n=== Processing outcome: %s ===\n", outcome_var))

    # Filter data to non-missing for this outcome
    outcome_data <- data_clean[!is.na(get(outcome_var))]

    # [EXISTING GRID LOGIC GOES HERE]
    # - Build specification grid
    # - Loop over specifications
    # - Call run_power_analysis()
    # - Collect results

    # Add outcome identifier to results
    outcome_results$final_power[, outcome := outcome_var]
    outcome_results$power_summary[, outcome := outcome_var]
    if (!is.null(outcome_results$specifications)) {
      outcome_results$specifications[, outcome := outcome_var]
    }

    # Store results
    all_final_power[[outcome_var]] <- outcome_results$final_power
    all_power_summary[[outcome_var]] <- outcome_results$power_summary
    all_specifications[[outcome_var]] <- outcome_results$specifications
  }

  # Combine results across outcomes
  final_power_combined <- rbindlist(all_final_power, use.names = TRUE, fill = TRUE)
  power_summary_combined <- rbindlist(all_power_summary, use.names = TRUE, fill = TRUE)
  specifications_combined <- rbindlist(all_specifications, use.names = TRUE, fill = TRUE)

  return(list(
    final_power = final_power_combined,
    power_summary = power_summary_combined,
    specifications = specifications_combined
  ))
}
```

**Step 3: Update documentation**

Update the @param for outcome:
```r
#' @param outcome Character vector. Outcome variable name(s). Can be single outcome or vector of multiple outcomes.
```

Add to @details:
```r
#' When multiple outcomes are provided, the function loops over each outcome,
#' runs the full grid search, and combines results with an 'outcome' identifier column.
```

**Step 4: Test single outcome (backward compatibility)**

```r
# Should work exactly as before
single_result <- run_power_grid(
  data_clean = test_data,
  unit_var = "state_fips",
  group_var = "year_passed",
  time_var = "year",
  rel_pass_var = "rel_pass",
  treat_ind_var = "law_pass",
  outcome = "outcome",  # Single outcome
  pta_type = "cs",
  percent_effect = 0.10,
  n_sims = 2
)

print("Single outcome test:")
print(names(single_result))
print(head(single_result$final_power))
```

**Step 5: Test multiple outcomes**

```r
# Add second outcome to test data
test_data[, outcome2 := rnorm(150)]

# Test with multiple outcomes
multi_result <- run_power_grid(
  data_clean = test_data,
  unit_var = "state_fips",
  group_var = "year_passed",
  time_var = "year",
  rel_pass_var = "rel_pass",
  treat_ind_var = "law_pass",
  outcome = c("outcome", "outcome2"),  # Multiple outcomes
  pta_type = "cs",
  percent_effect = 0.10,
  n_sims = 2
)

print("Multiple outcome test:")
print(unique(multi_result$final_power$outcome))
print(nrow(multi_result$final_power))  # Should be ~2x single outcome
```

**Step 6: Rebuild package and test**

```bash
cd /home/zjelveh/Dropbox/research/base_rate_fallacy/repo/staggeredpower
R CMD build .
R CMD INSTALL staggeredpower_0.1.0.tar.gz
```

**Step 7: Commit**

```bash
git add R/run_power_grid.R
git commit -m "feat: enable run_power_grid to handle multiple outcomes

- Accept outcome as character vector
- Loop over outcomes internally
- Add outcome identifier column to results
- Combine results across outcomes
- Backward compatible with single outcome"
```

---

## Phase 3: Create Examples Folder

### Task 5: Create examples directory and copy config

**Files:**
- Create: `examples/pwr_config.yaml`
- Create: `examples/README.md`

**Step 1: Create examples directory**

```bash
mkdir -p examples
```

**Step 2: Copy config file**

```bash
cp /home/zjelveh/Dropbox/research/base_rate_fallacy/repo/base_rate_fallacy/code/paper_code/power_analysis/pwr_config.yaml examples/
```

**Step 3: Create examples README**

Create `examples/README.md`:

```markdown
# staggeredpower Examples

This folder contains example scripts demonstrating real-world usage of the staggeredpower package.

## Large-Scale Power Analysis

**File:** `large_scale_analysis.R`

Demonstrates config-driven power analysis across:
- Multiple data sources (NIBRS, SHR)
- Multiple outcomes per source
- Multiple PTA methods
- Multiple control specifications
- Range of effect sizes

**Configuration:** `pwr_config.yaml`

Defines the parameter grid:
- `analysis_level`: State vs county analysis
- `data_source`: Which datasets to analyze
- `use_controls`: Control variable combinations
- `enforce_pta`: PTA enforcement methods and controls
- `sim_effects`: Range of effect sizes to test
- `models_to_run`: Which DiD estimators to use
- `n_sims`: Monte Carlo iterations

**Usage:**

1. Prepare your data with required structure (see main README)
2. Adjust `pwr_config.yaml` for your analysis
3. Update data loading section in script
4. Run: `Rscript large_scale_analysis.R`

Results are saved to SQLite database for further analysis.
```

**Step 4: Commit**

```bash
git add examples/
git commit -m "docs: create examples folder with config

- Add examples/ directory structure
- Copy pwr_config.yaml from research repo
- Add examples README explaining usage"
```

---

### Task 6: Create large_scale_analysis.R example

**Files:**
- Create: `examples/large_scale_analysis.R`

**Step 1: Create the example script**

Create `examples/large_scale_analysis.R`:

```r
# Large-Scale Power Analysis Example
# Demonstrates using run_power_grid() with config-driven grid search

library(staggeredpower)
library(data.table)
library(yaml)
library(RSQLite)

#===============================================================================
# Configuration
#===============================================================================

# Load configuration from YAML
config <- read_yaml('pwr_config.yaml')
config_to_run <- config$full_config

# Database for results
db_path <- "power_analysis_results.sqlite"
db <- dbConnect(SQLite(), db_path)

#===============================================================================
# Data Loading
#===============================================================================

# MODIFY THIS SECTION FOR YOUR DATA
#
# You need to provide:
# - Panel data with required columns (see README)
# - Function to extract outcome names based on data source
#
# Example structure:
# source('path/to/create_datasets.R')
# datasets <- create_datasets()
# data_county <- datasets$county
# data_state <- datasets$state

# For demonstration, we'll use placeholder:
data_state <- NULL  # Replace with your state-level data
data_county <- NULL  # Replace with your county-level data

#===============================================================================
# Helper Functions
#===============================================================================

# Extract outcome variable names based on data source and filters
get_outcomes <- function(data, data_source, filters = list(sex = "female")) {
  if (data_source == "nibrs") {
    outcomes <- names(data)[grepl('y_nibrs', names(data))]
    # Filter to aggshare or 18+ outcomes
    outcomes <- outcomes[grepl('aggshare|18', outcomes)]
  } else if (data_source == "shr") {
    outcomes <- names(data)[grepl('y_shr', names(data))]
  } else {
    stop(sprintf("Unknown data source: %s", data_source))
  }

  # Apply filters
  if (!is.null(filters$sex)) {
    outcomes <- outcomes[grepl(filters$sex, outcomes)]
  }

  return(sort(outcomes))
}

# Filter to balanced panel (all units observed every year)
ensure_balanced_panel <- function(data, unit_var, time_var) {
  # Find units with complete time series (no gaps)
  max_diffs <- data[, .(max_diff = max(diff(sort(get(time_var))))), by = unit_var]
  complete_units <- max_diffs[max_diff == 1][[unit_var]]

  # Return filtered data
  return(data[get(unit_var) %in% complete_units])
}

#===============================================================================
# Main Analysis Loop
#===============================================================================

set.seed(1000)

# Iterate through configurations
for (analysis_level in config_to_run$analysis_level) {

  # Select appropriate data
  data <- if (analysis_level == 'state_fips') data_state else data_county

  if (is.null(data)) {
    warning(sprintf("Skipping %s - no data provided", analysis_level))
    next
  }

  cat(sprintf("\n========================================\n"))
  cat(sprintf("Analysis Level: %s\n", analysis_level))
  cat(sprintf("========================================\n\n"))

  for (data_source in config_to_run$data_source) {

    cat(sprintf("\n--- Data Source: %s ---\n", data_source))

    # Get outcomes for this data source
    outcomes <- get_outcomes(data, data_source, filters = list(sex = "female"))

    if (length(outcomes) == 0) {
      warning(sprintf("No outcomes found for %s", data_source))
      next
    }

    cat(sprintf("Found %d outcomes: %s\n",
                length(outcomes),
                paste(head(outcomes, 3), collapse = ", ")))

    # Ensure balanced panel
    data_filtered <- ensure_balanced_panel(data, analysis_level, "year")

    # Extract PTA configurations
    # Config structure: list of (method: [controls1, controls2, ...])
    for (pta_config in config_to_run$enforce_pta) {
      pta_method <- names(pta_config)
      enforce_controls_list <- pta_config[[pta_method]]

      cat(sprintf("\nPTA Method: %s\n", pta_method))

      # Run power grid for this configuration
      # Grid dimensions:
      # - outcomes: vector of outcome names
      # - pta_type: single method per call
      # - enforce_type: controls used in PTA enforcement
      # - controls: controls used in estimation
      # - percent_effect: vector of effect sizes
      # - models_to_run: vector of estimators

      tryCatch({

        results <- run_power_grid(
          data_clean = data_filtered,
          unit_var = analysis_level,
          group_var = "year_passed",
          time_var = "year",
          rel_pass_var = "rel_pass",
          treat_ind_var = "law_pass",

          # Multiple outcomes - processed in single call
          outcome = outcomes,

          # Year range (configurable)
          min_year = 1995,
          max_year = 2019,

          # PTA method
          pta_type = pta_method,

          # Grid dimensions
          enforce_type = enforce_controls_list,  # List of control sets for PTA
          controls = config_to_run$use_controls,  # List of control sets for estimation
          percent_effect = seq(
            config_to_run$sim_effects$min,
            config_to_run$sim_effects$max,
            config_to_run$sim_effects$skip
          ),

          # Fixed parameters
          models_to_run = config_to_run$models_to_run,
          n_sims = config_to_run$n_sims,

          # Parallelization
          parallel = TRUE,
          n_cores = 50
        )

        # Add metadata columns
        results$final_power[, `:=`(
          data_source = data_source,
          analysis_level = analysis_level
        )]

        results$power_summary[, `:=`(
          data_source = data_source,
          analysis_level = analysis_level
        )]

        # Write to database with retry logic
        max_attempts <- 5
        for (attempt in 1:max_attempts) {
          tryCatch({
            dbWriteTable(db, "final_power", results$final_power, append = TRUE)
            dbWriteTable(db, "power_summary", results$power_summary, append = TRUE)
            cat(sprintf("  ✓ Saved %d power results\n", nrow(results$final_power)))
            break
          }, error = function(e) {
            if (attempt == max_attempts) {
              stop(sprintf("Failed to write to database after %d attempts: %s",
                          max_attempts, e$message))
            }
            Sys.sleep(runif(1, 0.1, 0.5) * attempt)
          })
        }

      }, error = function(e) {
        warning(sprintf("Error in grid for %s/%s: %s",
                       data_source, pta_method, e$message))
      })
    }
  }
}

#===============================================================================
# Cleanup
#===============================================================================

dbDisconnect(db)
cat(sprintf("\n\nAnalysis complete. Results saved to: %s\n", db_path))
cat("Use SQL or R to query results:\n")
cat("  db <- dbConnect(SQLite(), 'power_analysis_results.sqlite')\n")
cat("  results <- dbReadTable(db, 'power_summary')\n")
```

**Step 2: Commit**

```bash
git add examples/large_scale_analysis.R
git commit -m "docs: add large-scale power analysis example

- Config-driven grid search across multiple dimensions
- Handles multiple outcomes per data source
- Demonstrates run_power_grid with real workflow
- Includes data filtering and database storage
- Comprehensive comments for users"
```

---

## Phase 4: Update Documentation

### Task 7: Update README with Examples section

**Files:**
- Modify: `README.md`

**Step 1: Add Examples section to README**

After the "Typical Workflow" section (around line 313), add:

```markdown
## Examples

See the `examples/` folder for complete working examples:

### Large-Scale Power Analysis

**File:** [`examples/large_scale_analysis.R`](examples/large_scale_analysis.R)

Demonstrates config-driven power analysis across multiple:
- Data sources (NIBRS, SHR)
- Outcomes (automatically filtered per source)
- PTA methods (CS, imputation)
- Control specifications
- Effect sizes

**Key features:**
- Uses `run_power_grid()` with multiple outcomes
- YAML configuration file for parameter grid
- Balanced panel filtering
- Parallel processing
- SQLite database storage

**Configuration:** [`examples/pwr_config.yaml`](examples/pwr_config.yaml)

See the example README for usage instructions: [`examples/README.md`](examples/README.md)
```

**Step 2: Update Quick Start with year parameters**

In the Quick Start section (around line 14-36), update the example:

```r
library(staggeredpower)

# Minimal example with your panel data
results <- run_power_analysis(
  data_clean = your_data,
  unit_var = "state_fips",
  group_var = "year_passed",
  time_var = "year",
  rel_pass_var = "rel_pass",
  treat_ind_var = "law_pass",
  outcome = "outcome_var",
  pta_type = "cs",
  percent_effect = 0.10,
  n_sims = 100,
  min_year = 1995,    # Optional: filter to year range
  max_year = 2019     # Optional: filter to year range
)

# Calculate power
power <- results$final_power[, .(
  power = mean(abs(att/se) > 1.96)
), by = model]
print(power)
```

**Step 3: Update Grid Search section**

In the "Grid Search (Multiple Specifications)" section, add multiple outcomes example:

```r
# Test multiple outcomes simultaneously
grid_results <- run_power_grid(
  data_clean = data,
  unit_var = "state_fips",
  group_var = "year_passed",
  time_var = "year",
  rel_pass_var = "rel_pass",
  treat_ind_var = "law_pass",

  # Multiple outcomes
  outcome = c("dv_rate", "dv_count", "dv_share"),

  # Year range
  min_year = 1995,
  max_year = 2019,

  # Grid parameters
  pta_type = c("cs", "imputation"),
  percent_effect = seq(0.05, 0.20, 0.05),
  n_sims = 100
)

# Results include outcome identifier
print(unique(grid_results$final_power$outcome))
```

**Step 4: Commit**

```bash
git add README.md
git commit -m "docs: update README with examples and new features

- Add Examples section linking to examples/ folder
- Update Quick Start with year range parameters
- Add multiple outcomes example to Grid Search
- Document new capabilities"
```

---

### Task 8: Rebuild package and final test

**Files:**
- Rebuild entire package

**Step 1: Update NAMESPACE and documentation**

```bash
cd /home/zjelveh/Dropbox/research/base_rate_fallacy/repo/staggeredpower
R -e "devtools::document()"
```

**Step 2: Build package**

```bash
R CMD build .
```

Expected output: `staggeredpower_0.1.0.tar.gz` created

**Step 3: Install and test**

```bash
R CMD INSTALL staggeredpower_0.1.0.tar.gz
```

**Step 4: Run comprehensive test**

```r
library(staggeredpower)
library(data.table)

# Create test dataset
test_data <- data.table(
  state_fips = rep(1:10, each = 30),
  year = rep(1990:2019, times = 10),
  year_passed = rep(c(2000, 2005, NA, 2010, 2015, 2000, 2005, NA, 2010, 2015), each = 30),
  law_pass = 0,
  rel_pass = 0,
  outcome1 = rnorm(300, 50, 10),
  outcome2 = rnorm(300, 100, 20),
  unemp_rate = rnorm(300, 5, 1)
)
test_data[year >= year_passed & !is.na(year_passed), law_pass := 1]
test_data[, rel_pass := year - year_passed]

# Test 1: Single outcome with year range
cat("\n=== Test 1: Single outcome with year range ===\n")
r1 <- run_power_analysis(
  data_clean = test_data,
  unit_var = "state_fips",
  group_var = "year_passed",
  time_var = "year",
  rel_pass_var = "rel_pass",
  treat_ind_var = "law_pass",
  outcome = "outcome1",
  pta_type = "cs",
  percent_effect = 0.10,
  n_sims = 2,
  min_year = 1995,
  max_year = 2019
)
print(paste("Rows:", nrow(r1$final_power)))

# Test 2: Multiple outcomes in grid
cat("\n=== Test 2: Multiple outcomes in grid ===\n")
r2 <- run_power_grid(
  data_clean = test_data,
  unit_var = "state_fips",
  group_var = "year_passed",
  time_var = "year",
  rel_pass_var = "rel_pass",
  treat_ind_var = "law_pass",
  outcome = c("outcome1", "outcome2"),
  pta_type = c("cs", "imputation"),
  percent_effect = c(0.05, 0.10),
  n_sims = 2,
  min_year = 1995,
  max_year = 2019
)
print(paste("Unique outcomes:", paste(unique(r2$final_power$outcome), collapse = ", ")))
print(paste("Total rows:", nrow(r2$final_power)))

# Test 3: No year range (use all data)
cat("\n=== Test 3: No year range ===\n")
r3 <- run_power_analysis(
  data_clean = test_data,
  unit_var = "state_fips",
  group_var = "year_passed",
  time_var = "year",
  rel_pass_var = "rel_pass",
  treat_ind_var = "law_pass",
  outcome = "outcome1",
  pta_type = "cs",
  percent_effect = 0.10,
  n_sims = 2
)
print(paste("Rows:", nrow(r3$final_power)))

cat("\n=== All tests passed! ===\n")
```

**Step 5: Commit**

```bash
git add NAMESPACE man/
git commit -m "docs: rebuild package documentation

- Update NAMESPACE for new exports
- Regenerate man pages with new parameters
- Package builds and installs successfully"
```

---

### Task 9: Push changes and create release

**Files:**
- Push all commits

**Step 1: Review changes**

```bash
git log --oneline -10
```

Should see commits for:
- Year parameters (3 commits)
- Multiple outcomes (1 commit)
- Examples folder (3 commits)
- Documentation (2 commits)

**Step 2: Push to remote**

```bash
git push origin main
```

**Step 3: Create release tag**

```bash
git tag -a v0.2.0 -m "Release v0.2.0: Multiple outcomes and configurable year ranges

New Features:
- Multiple outcome support in run_power_grid()
- Configurable year range filtering (min_year/max_year)
- Large-scale analysis example with config file

Breaking Changes:
- None (backward compatible)

See docs/plans/2025-01-08-grid-function-enhancement-design.md for details"

git push origin v0.2.0
```

**Step 4: Update GitHub release**

Create release on GitHub with:
- Tag: v0.2.0
- Title: "v0.2.0 - Multiple Outcomes and Year Ranges"
- Description: Copy from tag message
- Attach: `staggeredpower_0.1.0.tar.gz`

---

## Testing Checklist

Before considering this complete:

- [ ] `run_power_analysis()` works with min_year/max_year
- [ ] `run_power_analysis()` works with NULL years (all data)
- [ ] `estimate_models()` filters by year range correctly
- [ ] `run_power_grid()` works with single outcome (backward compat)
- [ ] `run_power_grid()` works with multiple outcomes
- [ ] Multiple outcomes get outcome identifier column
- [ ] Examples folder has all files
- [ ] Example code runs without errors
- [ ] README links to examples correctly
- [ ] Package builds without warnings
- [ ] All commits pushed to main
- [ ] Release tagged

## Success Criteria

Implementation is complete when:
1. All hardcoded years removed from codebase
2. Year range parameters optional and working
3. `run_power_grid()` accepts and processes outcome vectors
4. `examples/` folder demonstrates real-world usage
5. README updated with examples and new features
6. Package version bumped and released
7. All tests pass

---

**Estimated Time:** 4-6 hours
- Phase 1: 1-2 hours (year parameters)
- Phase 2: 1-2 hours (multiple outcomes)
- Phase 3: 1 hour (examples)
- Phase 4: 1 hour (documentation and testing)
