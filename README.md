# staggeredpower

Simulated power analysis for heterogeneity-robust difference-in-difference designs.

## Installation

```r
# install.packages("devtools")
devtools::install_github("zjelveh/staggeredpower")
```

## Quick Start

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
  n_sims = 100
)

# Calculate power
power <- results$final_power[, .(
  power = mean(abs(att/se) > 1.96)
), by = model]
print(power)
```

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
  percent_effect = 0.10,          # Simulate 10% effect

  # Number of simulations
  n_sims = 100,                   # Monte Carlo iterations

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

**When to use:** You want to explore how power changes across different effect sizes, PTA methods, or control specifications.

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

**Parallel processing:**

```r
# Much faster for large grids
grid_results <- run_power_grid(
  data_clean = data,
  unit_var = "state_fips",
  group_var = "year_passed",
  time_var = "year",
  rel_pass_var = "rel_pass",
  treat_ind_var = "law_pass",
  outcome = "dv_rate",
  pta_type = c("cs", "imputation"),
  percent_effect = seq(0.05, 0.30, 0.05),
  n_sims = 100,
  parallel = TRUE,
  n_cores = 10
)
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

The CS method may drop units that violate parallel trends (negative predicted outcomes). This is tracked in:
- `n_dropped_units`: How many units dropped per simulation
- `share_units_dropped`: Proportion of sample dropped

More violations → smaller effective sample → lower power.

## Advanced Options

**Using controls in PTA enforcement:**

```r
results <- run_power_analysis(
  data_clean = data,
  unit_var = "state_fips",
  group_var = "year_passed",
  time_var = "year",
  rel_pass_var = "rel_pass",
  treat_ind_var = "law_pass",
  outcome = "dv_rate",
  pta_type = "cs",
  enforce_type = c("unemp_rate"),  # Use controls when generating counterfactuals
  controls = c("unemp_rate"),       # Also use in estimation
  percent_effect = 0.10,
  n_sims = 100
)
```

**Log-transforming outcomes:**

```r
results <- run_power_analysis(
  data_clean = data,
  unit_var = "state_fips",
  group_var = "year_passed",
  time_var = "year",
  rel_pass_var = "rel_pass",
  treat_ind_var = "law_pass",
  outcome = "dv_rate",
  transform_outcome = "log",  # Log transform before analysis
  pta_type = "cs",
  percent_effect = 0.10,
  n_sims = 100
)
```

## Typical Workflow

```r
# 1. Quick check with single specification
quick_check <- run_power_analysis(
  data_clean = data,
  unit_var = "state_fips",
  group_var = "year_passed",
  time_var = "year",
  rel_pass_var = "rel_pass",
  treat_ind_var = "law_pass",
  outcome = "dv_rate",
  pta_type = "cs",
  percent_effect = 0.10,
  n_sims = 50  # Fewer sims for quick check
)

# Verify it runs without errors
print(quick_check$final_power[, .(mean(att), mean(se))])

# 2. Run full grid search
full_results <- run_power_grid(
  data_clean = data,
  unit_var = "state_fips",
  group_var = "year_passed",
  time_var = "year",
  rel_pass_var = "rel_pass",
  treat_ind_var = "law_pass",
  outcome = "dv_rate",
  pta_type = c("cs", "imputation"),
  percent_effect = seq(0.05, 0.30, 0.05),
  controls = list(NULL, c("unemp_rate")),
  n_sims = 500,  # More sims for final results
  parallel = TRUE
)

# 3. Analyze and export results
power_summary <- full_results$power_summary
fwrite(power_summary, "power_analysis_results.csv")
```

## Citation

If you use this package, please cite:

[Your paper citation]

## License

MIT License

## Contact

Zubin Jelveh - zjelveh@umd.edu
