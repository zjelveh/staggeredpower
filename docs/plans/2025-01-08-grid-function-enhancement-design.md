# Grid Function Enhancement and Example Migration

**Date:** 2025-01-08
**Status:** Approved
**Goal:** Extend `run_power_grid()` to handle multiple outcomes and configurable year ranges, then migrate large-scale analysis code to examples/

## Background

Current power analysis code in `base_rate_fallacy/code/paper_code/power_analysis/` uses nested loops to iterate through parameter combinations manually. The new `run_power_grid()` function should handle this, but needs enhancements:

1. Accept multiple outcomes (currently single outcome only)
2. Make year ranges configurable (currently hardcoded 1995-2019)
3. Provide example of large-scale analysis workflow

## Requirements

### Functional
- `run_power_grid()` accepts vector of outcomes, loops internally
- Year filtering controlled by `min_year` and `max_year` parameters
- Results from multiple outcomes combined with outcome identifier column
- Example code demonstrates real-world usage with config file

### Non-functional
- Backward compatible: existing single-outcome calls still work
- Year parameters optional (NULL = use all data)
- Example code well-documented for users

## Design

### 1. Function Signature Changes

**run_power_grid() - Enhanced:**
```r
run_power_grid <- function(
  data_clean,
  unit_var,
  group_var,
  time_var,
  rel_pass_var,
  treat_ind_var,
  outcome,              # NOW ACCEPTS VECTOR
  min_year = NULL,      # NEW: optional minimum year
  max_year = NULL,      # NEW: optional maximum year
  pta_type = "cs",
  enforce_type = NULL,
  controls = NULL,
  percent_effect = 0.10,
  models_to_run = c("cs", "imputation"),
  n_sims = 100,
  parallel = FALSE,
  n_cores = NULL
)
```

**run_power_analysis() - Updated:**
```r
run_power_analysis <- function(
  data_clean,
  unit_var,
  group_var,
  time_var,
  rel_pass_var,
  treat_ind_var,
  controls = NULL,
  outcome,
  transform_outcome = NULL,
  pta_type,
  enforce_type = NULL,
  percent_effect,
  models_to_run = c('cs', 'imputation', 'twfe'),
  n_sims = 100,
  min_year = NULL,      # NEW
  max_year = NULL       # NEW
)
```

### 2. Year Filtering Implementation

Replace all hardcoded `between(year, 1995, 2019)` with:

```r
filter_by_years <- function(data, time_var, min_year, max_year) {
  if (!is.null(min_year) && !is.null(max_year)) {
    return(data[get(time_var) >= min_year & get(time_var) <= max_year])
  } else if (!is.null(min_year)) {
    return(data[get(time_var) >= min_year])
  } else if (!is.null(max_year)) {
    return(data[get(time_var) <= max_year])
  } else {
    return(data)
  }
}
```

**Locations to update:**
- `R/power_analysis.R:34` - `data_clean_full = data_clean[between(year, 1995, 2019)]`
- `R/power_analysis.R:71` - `(na_error == 1 & get(time_var) < 2019)`
- `R/estimate_models.R:26` - `analysis_data <- data[between(year, 1995, 2019)]`

### 3. Multiple Outcome Handling

**In run_power_grid():**

```r
# Check if outcome is vector
if (length(outcome) == 1) {
  # Single outcome - existing behavior
  return(run_grid_single_outcome(...))
}

# Multiple outcomes - loop and combine
all_results <- list()

for (outcome_var in outcome) {
  cat(sprintf("Processing outcome: %s\n", outcome_var))

  # Run grid for this outcome
  outcome_results <- run_grid_single_outcome(
    data_clean = data_clean,
    outcome = outcome_var,
    # ... other params
  )

  # Add outcome identifier column
  outcome_results$final_power[, outcome := outcome_var]
  outcome_results$power_summary[, outcome := outcome_var]
  if (!is.null(outcome_results$specifications)) {
    outcome_results$specifications[, outcome := outcome_var]
  }

  all_results[[outcome_var]] <- outcome_results
}

# Combine results across outcomes
combined <- list(
  final_power = rbindlist(lapply(all_results, function(x) x$final_power)),
  power_summary = rbindlist(lapply(all_results, function(x) x$power_summary)),
  specifications = rbindlist(lapply(all_results, function(x) x$specifications))
)

return(combined)
```

### 4. Repository Structure

**New structure:**
```
staggeredpower/
├── R/
│   ├── run_power_grid.R      (enhanced)
│   ├── power_analysis.R      (add year params)
│   ├── estimate_models.R     (add year params)
│   └── ...
├── examples/
│   ├── large_scale_analysis.R     (migrated from base_rate_fallacy)
│   └── pwr_config.yaml             (copied from base_rate_fallacy)
├── docs/
│   └── plans/
│       └── 2025-01-08-grid-function-enhancement-design.md
└── README.md                  (add Examples section)
```

**Example code (examples/large_scale_analysis.R):**
```r
# Large-Scale Power Analysis Example
# Demonstrates using run_power_grid() with config-driven grid search

library(staggeredpower)
library(data.table)
library(yaml)
library(RSQLite)

# Load configuration
config <- read_yaml('pwr_config.yaml')
config_to_run <- config$full_config

# Load your data
# source('path/to/create_datasets.R')
# datasets <- create_datasets()
# data <- datasets$state  # or datasets$county

# Extract outcomes from data based on data source
get_outcomes <- function(data, data_source, sex = "female") {
  if (data_source == "nibrs") {
    outcomes <- names(data)[grepl('y_nibrs', names(data))]
    outcomes <- outcomes[grepl('aggshare|18', outcomes)]
  } else if (data_source == "shr") {
    outcomes <- names(data)[grepl('y_shr', names(data))]
  }
  outcomes <- outcomes[grepl(sex, outcomes)]
  return(sort(outcomes))
}

# Run grid search for each configuration
for (data_source in config_to_run$data_source) {
  outcomes <- get_outcomes(data, data_source)

  # Run power grid for ALL outcomes at once
  results <- run_power_grid(
    data_clean = data,
    unit_var = config_to_run$analysis_level,
    group_var = "year_passed",
    time_var = "year",
    rel_pass_var = "rel_pass",
    treat_ind_var = "law_pass",
    outcome = outcomes,                    # VECTOR of outcomes
    min_year = 1995,                       # Configurable
    max_year = 2019,                       # Configurable
    pta_type = config_to_run$enforce_pta,
    controls = config_to_run$use_controls,
    percent_effect = seq(
      config_to_run$sim_effects$min,
      config_to_run$sim_effects$max,
      config_to_run$sim_effects$skip
    ),
    models_to_run = config_to_run$models_to_run,
    n_sims = config_to_run$n_sims,
    parallel = TRUE,
    n_cores = 50
  )

  # Save results to database
  db <- dbConnect(SQLite(), "power_analysis_results.sqlite")
  dbWriteTable(db, "results", results$final_power, append = TRUE)
  dbWriteTable(db, "power_summary", results$power_summary, append = TRUE)
  dbDisconnect(db)
}
```

## Implementation Plan

### Phase 1: Fix Hardcoded Years
1. Add `min_year`, `max_year` parameters to `run_power_analysis()`
2. Add `min_year`, `max_year` parameters to `estimate_models()`
3. Replace hardcoded `between(year, 1995, 2019)` in both functions
4. Update `run_power_grid()` to pass year params through
5. Test with explicit years and NULL (use all data)

### Phase 2: Extend run_power_grid()
1. Modify `run_power_grid()` signature to accept outcome vector
2. Add internal loop over outcomes
3. Add outcome identifier column to results
4. Combine results with rbindlist
5. Test with single and multiple outcomes

### Phase 3: Create Examples
1. Create `examples/` directory
2. Migrate and adapt `run_power_analysis_test.R` → `large_scale_analysis.R`
3. Copy `pwr_config.yaml` to examples/
4. Document example code with comments
5. Test example end-to-end

### Phase 4: Update Documentation
1. Update README.md with Examples section
2. Link to examples/ from main README
3. Document new parameters in function documentation
4. Rebuild package and test

## Testing Strategy

**Unit tests:**
- Year filtering with NULL/min/max combinations
- Single vs multiple outcomes return same results for one outcome
- Outcome column correctly added to all result tables

**Integration tests:**
- Run example code end-to-end
- Verify results match manual loop approach
- Check parallel processing works with multiple outcomes

## Rollout

1. Commit design document
2. Implement Phase 1 (years)
3. Test and commit
4. Implement Phase 2 (multiple outcomes)
5. Test and commit
6. Implement Phase 3 (examples)
7. Test and commit
8. Update documentation (Phase 4)
9. Final release

## Success Criteria

- [ ] `run_power_grid()` accepts outcome vector
- [ ] Year ranges configurable via parameters
- [ ] All hardcoded years removed
- [ ] Example demonstrates real-world usage
- [ ] README updated with examples section
- [ ] Tests pass
- [ ] Package builds without errors
