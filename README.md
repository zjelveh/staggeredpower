# staggeredpower

Power analysis for staggered difference-in-differences designs with parallel trends enforcement.

## Installation

```r
# install.packages("devtools")
devtools::install_github("zjelveh/staggeredpower")
```

## Quick Start

```r
library(staggeredpower)

# Assuming you have panel data with:
# - state_fips: state identifier
# - year: time period
# - year_passed: year treatment was adopted
# - law_pass: treatment indicator (0/1)

results <- run_power_analysis(
  data_clean = your_data,
  unit_var = "state_fips",
  group_var = "year_passed",
  time_var = "year",
  outcome = "outcome_var",
  pta_type = "cs",
  percent_effect = 0.10,
  n_sims = 100
)

# Check power
power <- mean(abs(results$final_power$att / results$final_power$se) > 1.96)
print(paste("Power:", round(power, 3)))
```

## Tutorial: Running a Power Analysis

### What does this package do?

Staggered DiD power analysis involves three steps:

1. **Generate counterfactuals** under parallel trends (what would have happened without treatment)
2. **Simulate treatment effects** of a given magnitude
3. **Estimate power** by running DiD estimators and checking statistical significance

This package handles all three steps.

### Complete Example

**Step 1: Prepare your data**

Your data should be a balanced panel with these columns:
- Unit identifier (e.g., `state_fips`, `county_name`)
- Time variable (e.g., `year`)
- Treatment cohort (e.g., `year_passed` - year unit adopted treatment)
- Treatment indicator (e.g., `law_pass` - 0 before, 1 after adoption)
- Outcome variable(s)
- (Optional) Control variables

```r
library(data.table)
library(staggeredpower)

# Load your data
# data <- fread("your_data.csv")

# Ensure balanced panel - all units observed every year
# Filter to units with complete time series
```

**Step 2: Run power analysis for a single specification**

```r
results <- run_power_analysis(
  data_clean = data,

  # Panel structure
  unit_var = "state_fips",        # Unit ID column name
  group_var = "year_passed",      # Treatment cohort column name
  time_var = "year",              # Time column name

  # Outcome
  outcome = "dv_rate",            # Outcome column name

  # PTA enforcement method
  pta_type = "cs",                # "cs" (Callaway-Sant'Anna) or "imputation"

  # Simulated effect size
  percent_effect = 0.10,          # Simulate 10% effect (0.10 = 10%)

  # Number of simulations
  n_sims = 100,                   # More = more precise, but slower

  # Optional: controls and estimators
  controls = c("unemp_rate", "crime_rate"),
  models_to_run = c("cs", "imputation")
)
```

**Key parameters explained:**

- **`pta_type`**: Method for generating counterfactuals under parallel trends
  - `"cs"`: Uses Callaway-Sant'Anna's did package approach (recommended)
  - `"imputation"`: Uses two-way fixed effects imputation

- **`percent_effect`**: Treatment effect to simulate, as proportion of baseline outcome
  - Example: If baseline outcome = 5.0, `percent_effect = 0.10` simulates effect = 0.5

- **`n_sims`**: Number of Monte Carlo simulations
  - 100 is usually sufficient for power estimates
  - Use more (500+) for publication-quality results

- **`controls`**: Optional control variables
  - These are used in both PTA enforcement and estimation
  - Can improve power if controls explain outcome variation

**Step 3: Interpret results**

The function returns a list with two components:

```r
names(results)
# [1] "final_vio"   "final_power"
```

**`final_power`**: Main results (data.table with one row per simulation)

```r
library(data.table)

# View first few rows
head(results$final_power)

# Columns include:
# - att: estimated treatment effect
# - se: standard error
# - model: which estimator ("cs", "imputation", "twfe")
# - percent_effect: simulated effect size
# - n_dropped_units: units dropped due to PTA violations
# - share_units_dropped: proportion of sample dropped
```

**Calculate power:**

```r
# Power = proportion of simulations with statistically significant effects
power_by_model <- results$final_power[, .(
  power = mean(abs(att/se) > 1.96),  # % of sims with |t-stat| > 1.96
  mean_att = mean(att),               # Average estimated effect
  mean_se = mean(se),                 # Average standard error
  mean_units_dropped = mean(n_dropped_units)
), by = model]

print(power_by_model)

#    model power mean_att mean_se mean_units_dropped
# 1:    cs  0.82    -0.51    0.24                3.2
# 2:  imputation  0.76    -0.48    0.26                0.0
```

**Interpretation:**
- Power of 0.82 with CS estimator = 82% chance of detecting the effect
- CS drops ~3 units on average due to PTA violations
- Imputation doesn't drop units but has slightly lower power

**Check PTA violations:**

```r
# final_vio shows which units violated parallel trends in which simulations
head(results$final_vio)

# Summary
violation_summary <- results$final_vio[, .N, by = .(unit, iteration)][, .(
  times_violated = .N
), by = unit][order(-times_violated)]

print(violation_summary)
# Shows which units frequently violate PTA
```

## Grid Search Across Multiple Specifications

For testing power across many parameter combinations, use `run_power_grid()`:

```r
# Test multiple effect sizes and methods simultaneously
grid_results <- run_power_grid(
  data_clean = data,
  unit_var = "state_fips",
  group_var = "year_passed",
  time_var = "year",
  outcome = "dv_rate",

  # Vectors of parameters to test
  pta_type = c("cs", "imputation"),
  percent_effect = seq(0.05, 0.20, 0.05),  # Test 5%, 10%, 15%, 20%

  # Other parameters stay constant
  n_sims = 100
)

# View power summary across all specifications
print(grid_results$power_summary)
```

**Grid search with multiple control sets:**

```r
grid_results <- run_power_grid(
  data_clean = data,
  unit_var = "state_fips",
  group_var = "year_passed",
  time_var = "year",
  outcome = "dv_rate",

  # Test different control variable combinations
  controls = list(
    NULL,                                    # No controls
    c("unemp_rate"),                        # Unemployment only
    c("unemp_rate", "crime_rate")           # Both controls
  ),

  # Test multiple effect sizes
  percent_effect = c(0.10, 0.15, 0.20),

  n_sims = 100
)

# Results contain all combinations
# 3 control sets × 3 effect sizes × 2 models = 18 specifications
```

**Parallel processing for faster grid search:**

```r
# Run grid search in parallel (much faster for large grids)
grid_results <- run_power_grid(
  data_clean = data,
  unit_var = "state_fips",
  group_var = "year_passed",
  time_var = "year",
  outcome = "dv_rate",
  pta_type = c("cs", "imputation"),
  percent_effect = seq(0.05, 0.30, 0.05),
  controls = list(NULL, c("unemp_rate")),
  n_sims = 100,
  parallel = TRUE,
  n_cores = 10  # Use 10 cores
)
```

**Grid search output:**

`run_power_grid()` returns three components:

```r
names(grid_results)
# [1] "final_power"      "power_summary"    "specifications"
```

- **`final_power`**: Individual simulation results for all specifications
- **`power_summary`**: Aggregated power statistics by specification
- **`specifications`**: Grid of all parameter combinations tested

**Visualize power curves:**

```r
library(ggplot2)

# Power by effect size
ggplot(grid_results$power_summary,
       aes(x = percent_effect, y = power, color = pta_type, linetype = controls)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0.80, linetype = "dashed") +
  labs(title = "Statistical Power by Effect Size",
       x = "Simulated Effect Size (%)",
       y = "Power",
       color = "PTA Method",
       linetype = "Controls") +
  theme_minimal()
```

## Advanced Usage

**Using different PTA enforcement methods:**

```r
# Use controls when generating counterfactuals
results <- run_power_analysis(
  data_clean = data,
  unit_var = "state_fips",
  group_var = "year_passed",
  time_var = "year",
  outcome = "dv_rate",
  pta_type = "cs",
  enforce_type = c("unemp_rate"),  # Use controls in PTA enforcement
  controls = c("unemp_rate"),       # Also use in estimation
  percent_effect = 0.10,
  n_sims = 100
)
```

**Transform outcome variable:**

```r
# Log-transform outcome before power analysis
results <- run_power_analysis(
  data_clean = data,
  unit_var = "state_fips",
  group_var = "year_passed",
  time_var = "year",
  outcome = "dv_rate",
  transform_outcome = "log",  # Log transform
  pta_type = "cs",
  percent_effect = 0.10,
  n_sims = 100
)
```

## Typical Workflow

Here's a recommended workflow for power analysis:

```r
# 1. Quick check with single specification
quick_check <- run_power_analysis(
  data_clean = data,
  unit_var = "state_fips",
  group_var = "year_passed",
  time_var = "year",
  outcome = "dv_rate",
  pta_type = "cs",
  percent_effect = 0.10,
  n_sims = 50  # Fewer sims for quick check
)

# Check if it runs without errors
print(quick_check$final_power[, .(mean(att), mean(se))])

# 2. Run full grid search
full_results <- run_power_grid(
  data_clean = data,
  unit_var = "state_fips",
  group_var = "year_passed",
  time_var = "year",
  outcome = "dv_rate",
  pta_type = c("cs", "imputation"),
  percent_effect = seq(0.05, 0.30, 0.05),
  controls = list(NULL, c("unemp_rate"), c("unemp_rate", "crime_rate")),
  n_sims = 500,  # More sims for final results
  parallel = TRUE
)

# 3. Analyze and visualize results
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
