# staggeredpower

<!-- badges: start -->
[![R-CMD-check](https://github.com/zjelveh/staggeredpower/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/zjelveh/staggeredpower/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Simulated power analysis for heterogeneity-robust difference-in-difference designs.

## Installation

```r
# install.packages("remotes")
remotes::install_github("zjelveh/staggeredpower")
```

### Optional Dependencies

staggeredpower uses a pluggable adapter architecture. Install the estimators you need:

```r
# Callaway-Sant'Anna
install.packages("did")

# Gardner Two-Stage DiD
install.packages("did2s")

# Borusyak-Jaravel-Spiess Imputation
install.packages("didimputation")

# Parallel processing (recommended for large simulations)
install.packages(c("foreach", "doParallel"))
```

## Optional: Parallel Processing

Registering a parallel backend can significantly speed up Monte Carlo simulations. The package automatically falls back to sequential processing if no backend is registered.

```r
library(doParallel)

# Register parallel backend BEFORE running power analysis
cl <- makeCluster(detectCores() - 1)  # Use all cores except 1
registerDoParallel(cl)

# ... run your power analysis ...

# Clean up when done
stopCluster(cl)
```

Without a parallel backend, functions will run sequentially using `lapply`. This is fine for small simulations but may be slow for large grids with many iterations.


## Usage

### Basic Power Analysis (Single Specification)

Use `run_power_analysis()` to estimate power for one set of parameters.

**What it does:**
1. Generates counterfactual outcomes under parallel trends assumption
2. Simulates treatment multipliers of a specified magnitude
3. Estimates DiD models and calculates statistical significance rates

**Data requirements:**

Your data should be a panel dataset with:
- Unit identifier (e.g., `state_fips`, `county_name`)
- Time variable (e.g., `year`)
- Treatment cohort (e.g., `year_passed` - year unit adopted treatment; use `0` or `NA` for never-treated units)
- Treatment indicator (e.g., `law_pass` - 0 before, 1 after adoption)
- Outcome variable(s)
- Relative time to treatment (e.g., `rel_pass` = `year` - `year_passed`)

Balanced panels are not required. By default, the package follows the underlying estimator defaults and assumes a balanced panel for Callaway-Sant'Anna estimation. If your panel is unbalanced, pass `allow_unbalanced_panel = TRUE`; this is forwarded to `did::att_gt()` for CS specifications. Other estimators may have their own support requirements, so check estimator-specific warnings and missing-result rows.

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

  # Simulated treatment multiplier
  percent_effect = 0.90,          # Simulate a 10% decline (post-treatment outcomes are 90% of counterfactual)

  # Number of simulations
  n_sims = 100,                   # Number of Monte Carlo draws

  # Optional: controls and estimators
  controls = c("unemp_rate"),
  models_to_run = c("cs", "imputation"),

  # Optional: unbalanced panels and stochastic outcome noise
  allow_unbalanced_panel = TRUE,
  noise_spec = list(engine = "iid")
)
```

**Key parameters:**

- **`pta_type`**: Method for generating counterfactuals
  - `"cs"`: Callaway-Sant'Anna approach (recommended)
  - `"imputation"`: Two-way fixed effects imputation

- **`percent_effect`**: Treatment multiplier applied to post-treatment counterfactual outcomes
  - `percent_effect = 1.00` means no effect
  - `percent_effect = 0.90` means a 10% decline
  - `percent_effect = 1.10` means a 10% increase

- **`n_sims`**: Number of Monte Carlo simulations
  - 100 for quick estimates, 500+ for publication results
  - With `noise_spec = list(engine = "none")`, simulations are deterministic and the package sets `n_sims = 1`

- **`controls`**: Control variables (optional)
  - Used in both PTA enforcement and estimation

- **`allow_unbalanced_panel`**: Whether to allow unbalanced panels in CS estimation
  - Default is `FALSE`, matching `did::att_gt()`
  - Set to `TRUE` for unbalanced state-year or agency-year panels

- **`noise_spec`**: Outcome-noise configuration
  - Default is `list(engine = "none")`, a deterministic benchmark
  - Use `list(engine = "iid")`, `list(engine = "ar1")`, or observation models such as `obs_model = "poisson"` / `"binomial"` for stochastic power calculations

**Interpreting results:**

```r
# Results structure
names(results)
# [1] "final_power"  "final_vio"

# Main results: one row per simulation
head(results$final_power)

# Calculate power by estimator
power_by_model <- results$final_power[, .(
  power = mean(abs(att/se) > 1.96),     # Share rejecting null at 5%
  mean_att = mean(att),                  # Average estimated effect
  mean_se = mean(se),                    # Average standard error
  mean_bound_errors = mean(n_bound_errors),
  mean_violating_units = mean(n_violating_units)
), by = model]

print(power_by_model)
#    model power mean_att mean_se mean_bound_errors mean_violating_units
# 1:    cs  0.82    -0.51    0.24              0.0                  0.0
# 2: imputation 0.76    -0.48    0.26              0.0                  0.0
```

Power of 0.82 means 82% chance of detecting the effect at 5% significance level.

### Grid Search (Multiple Specifications)

Use `run_power_grid()` to test power across many parameter combinations.

**When to use:** You want to explore how power changes across different treatment multipliers, PTA methods, or control specifications. **We recommend this approach**

**Example - Testing multiple treatment multipliers:**

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
  percent_effect = seq(0.80, 1.00, 0.05),  # Test 20%, 15%, 10%, 5%, 0% declines

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

  percent_effect = c(0.80, 0.85, 0.90),
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
       x = "Treatment Multiplier",
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
- `percent_effect`: Simulated treatment multiplier
- `n_bound_errors`: Number of counterfactual draws below the feasible lower bound before clipping
- `n_violating_units`: Number of units with diagnostic bound or missing-counterfactual issues
- `noise_engine`: Noise engine used for the simulation
- `obs_model`: Observation model used for stochastic draws

**`final_vio` table:**

Legacy diagnostics for parallel-trends/bound violations. In current versions, bound violations are tracked diagnostically and units are not dropped automatically from the power simulations.

```r
# Summarize frequent violators
violation_summary <- results$final_vio[, .N, by = .(unit, iteration)][, .(
  times_violated = .N
), by = unit][order(-times_violated)]

print(violation_summary)
```

**PTA and bound diagnostics:**

The CS and imputation PTA generators can produce infeasible or missing counterfactuals in sparse panels or with noisy outcomes. Current versions treat these as diagnostics rather than automatically dropping units:
- `n_bound_errors`: number of counterfactual values below zero before clipping
- `n_violating_units`: number of units with bound or missing-counterfactual diagnostics

Many diagnostics may indicate a fragile specification, even if the estimator still returns results.


## Vignettes

The package documentation is organized around vignettes rather than a separate `examples/` directory:

```r
vignette("getting-started", package = "staggeredpower")
vignette("advanced-usage", package = "staggeredpower")
```

`getting-started` introduces the main workflow. `advanced-usage` covers grid searches, noise engines, pre-trend tests, and custom estimator adapters.

