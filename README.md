# staggeredpower

Simulated power analysis for heterogeneity-robust difference-in-difference designs.

## Installation

```r
# install.packages("devtools")
devtools::install_github("zjelveh/staggeredpower")
```

## Important: Parallel Processing Setup

**This package requires parallel processing setup for reasonable performance.** Monte Carlo simulations use the `foreach` package with `%dopar%`, which requires a registered parallel backend.

### Required Setup (Do This First!)

```r
library(doParallel)

# Register parallel backend BEFORE running power analysis
cl <- makeCluster(detectCores() - 1)  # Use all cores except 1
registerDoParallel(cl)

# ... run your power analysis ...

# Clean up when done
stopCluster(cl)
```

**Without this setup:**
- Functions will run SEQUENTIALLY (very slow)
- You'll see a warning about missing parallel backend
- Analysis with `n_sims = 100` could take hours instead of minutes


## Usage

### Basic Power Analysis (Single Specification)

Use `run_power_analysis()` to estimate power for one set of parameters.

**What it does:**
1. Generates counterfactual outcomes under parallel trends assumption
2. Simulates treatment effects of a specified magnitude
3. Estimates DiD models and calculates statistical significance rates

**Data requirements:**

Your data should be a balanced panel with:
- Unit identifier (e.g., `state_fips`, `county_name`)
- Time variable (e.g., `year`)
- Treatment cohort (e.g., `year_passed` - year unit adopted treatment)
- Treatment indicator (e.g., `law_pass` - 0 before, 1 after adoption)
- Outcome variable(s)
- Relative time to treatment (e.g., `rel_pass` = `year` - `year_passed`)

**Example:**

```r
library(data.table)
library(staggeredpower)

# Load your data
# data <- fread("your_data.csv")

results <- run_power_analysis(
  data_clean = data,

  # Panel structure
  unit_var = "state_fips",        # Unit ID column name
  group_var = "year_passed",      # Treatment cohort column name
  time_var = "year",              # Time column name
  rel_pass_var = "rel_pass",      # Relative time column name
  treat_ind_var = "law_pass",     # Treatment indicator column name

  # Outcome
  outcome = "dv_rate",            # Outcome column name

  # PTA enforcement method
  pta_type = "cs",                # "cs" or "imputation"

  # Simulated effect size
  percent_effect = 0.10,          # Simulate 10% effect (this is a decline, new outcome is 10 percent of original outcome)

  # Number of simulations
  n_sims = 100,                   # Num iterations per simulated treatment ffect

  # Optional: controls and estimators
  controls = c("unemp_rate"),
  models_to_run = c("cs", "imputation")
)
```

**Key parameters:**

- **`pta_type`**: Method for generating counterfactuals
  - `"cs"`: Callaway-Sant'Anna approach (recommended)
  - `"imputation"`: Two-way fixed effects imputation

- **`percent_effect`**: Treatment effect to simulate as proportion of baseline outcome
  - Example: If baseline = 5.0, `percent_effect = 0.10` simulates effect = 0.5

- **`n_sims`**: Number of Monte Carlo simulations
  - 100 for quick estimates, 500+ for publication results

- **`controls`**: Control variables (optional)
  - Used in both PTA enforcement and estimation

**Interpreting results:**

```r
# Results structure
names(results)
# [1] "final_power"  "final_vio"

# Main results: one row per simulation
head(results$final_power)

# Calculate power by estimator
power_by_model <- results$final_power[, .(
  power = mean(abs(att/se) > 1.96),     # % rejecting null at 5%
  mean_att = mean(att),                  # Average estimated effect
  mean_se = mean(se),                    # Average standard error
  mean_units_dropped = mean(n_dropped_units)
), by = model]

print(power_by_model)
#    model power mean_att mean_se mean_units_dropped
# 1:    cs  0.82    -0.51    0.24                3.2
# 2:  imputation  0.76    -0.48    0.26                0.0
```

Power of 0.82 means 82% chance of detecting the effect at 5% significance level.

### Grid Search (Multiple Specifications)

Use `run_power_grid()` to test power across many parameter combinations.

**When to use:** You want to explore how power changes across different effect sizes, PTA methods, or control specifications. **We recommend this approach**

**Example - Testing multiple effect sizes:**

```r
grid_results <- run_power_grid(
  data_clean = data,
  unit_var = "state_fips",
  group_var = "year_passed",
  time_var = "year",
  rel_pass_var = "rel_pass",
  treat_ind_var = "law_pass",
  outcome = "dv_rate",

  # Vectors of parameters to test
  pta_type = c("cs", "imputation"),
  percent_effect = seq(0.05, 0.20, 0.05),  # Test 5%, 10%, 15%, 20%

  n_sims = 100
)

# View power summary
print(grid_results$power_summary)
```

**Example - Testing multiple control sets:**

```r
grid_results <- run_power_grid(
  data_clean = data,
  unit_var = "state_fips",
  group_var = "year_passed",
  time_var = "year",
  rel_pass_var = "rel_pass",
  treat_ind_var = "law_pass",
  outcome = "dv_rate",

  # Test different control combinations
  controls = list(
    NULL,                           # No controls
    c("unemp_rate"),               # Unemployment only
    c("unemp_rate", "crime_rate")  # Both controls
  ),

  percent_effect = c(0.10, 0.15, 0.20),
  n_sims = 100
)

# Results contain all combinations
# 3 control sets × 3 effect sizes = 9 specifications
```

**Grid search output:**

```r
names(grid_results)
# [1] "final_power"      "power_summary"    "specifications"

# final_power: Individual simulation results for all specs
# power_summary: Aggregated power statistics by specification
# specifications: Grid of all parameter combinations tested
```

**Visualizing power curves:**

```r
library(ggplot2)

ggplot(grid_results$power_summary,
       aes(x = percent_effect, y = power, color = pta_type)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0.80, linetype = "dashed") +
  labs(title = "Statistical Power by Effect Size",
       x = "Simulated Effect Size",
       y = "Power",
       color = "PTA Method") +
  theme_minimal()
```

## Understanding the Results

**`final_power` table:**

Contains one row per simulation with columns:
- `att`: Estimated treatment effect
- `se`: Standard error
- `model`: Which estimator ("cs", "imputation", "twfe")
- `percent_effect`: Simulated effect size
- `n_dropped_units`: Units dropped due to PTA violations
- `share_units_dropped`: Proportion of sample dropped

**`final_vio` table:**

Shows which units violated parallel trends in which simulations. Useful for understanding which units frequently cause violations.

```r
# Summarize frequent violators
violation_summary <- results$final_vio[, .N, by = .(unit, iteration)][, .(
  times_violated = .N
), by = unit][order(-times_violated)]

print(violation_summary)
```

**PTA violations:**

The CS and Imp methods may drop units that violate parallel trends. This is tracked in:
- `n_dropped_units`: How many units dropped per simulation
- `share_units_dropped`: Proportion of sample dropped

More violations → smaller effective sample → lower power.


## Examples

The `examples/` directory contains complete workflows demonstrating real-world usage:

### Large-Scale Analysis (`examples/large_scale_analysis.R`)

This example shows how to run power analysis at scale across multiple outcomes and configurations:

**Quick start:**

```r
# 1. Navigate to examples directory
setwd("path/to/staggeredpower/examples")

# 2. Load your data (see example for data structure requirements)
# source('your_data_loading_script.R')

# 3. Review and customize pwr_config.yaml for your analysis

# 4. Run the example
source('large_scale_analysis.R')

# 5. Query results from the SQLite database
db <- dbConnect(SQLite(), "power_analysis_results.sqlite")
results <- dbReadTable(db, "power_summary")
dbDisconnect(db)
```

**What it demonstrates:**
- Running grid search for multiple outcomes simultaneously (new feature!)
- Configurable year filtering with `min_year` and `max_year` parameters
- Config-driven parameter grids for reproducibility
- Database storage for large-scale results
- Retry logic for database writes
- Result querying and visualization

The example is extensively commented and can be adapted to your specific use case.

