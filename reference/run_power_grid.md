# Run Power Analysis Grid Search

Convenience wrapper around run_power_analysis() that runs power analysis
across multiple parameter combinations. Accepts vectors for key
parameters and iterates over all combinations.

## Usage

``` r
run_power_grid(
  data_clean,
  unit_var,
  group_var,
  time_var,
  rel_pass_var,
  treat_ind_var,
  outcome,
  pta_type = c("cs", "imputation"),
  enforce_type = list(NULL),
  percent_effect = seq(0.05, 0.2, 0.05),
  transform_outcome = list(NULL),
  controls = list(NULL),
  models_to_run = c("cs", "imputation"),
  n_sims = 100,
  min_year = NULL,
  max_year = NULL,
  parallel = FALSE,
  n_cores = NULL,
  noise_spec = list(engine = "none"),
  design_resample = "none"
)
```

## Arguments

- data_clean:

  Clean panel dataset (same as run_power_analysis)

- unit_var:

  Unit identifier column name (same as run_power_analysis)

- group_var:

  Treatment cohort column name (same as run_power_analysis)

- time_var:

  Time variable column name (same as run_power_analysis)

- rel_pass_var:

  Relative time to treatment column name (same as run_power_analysis)

- treat_ind_var:

  Treatment indicator column name (same as run_power_analysis)

- outcome:

  Character vector. Outcome variable column name(s). Can be single
  outcome or vector of multiple outcomes.

- pta_type:

  Vector of PTA types to test. Default: c("cs", "imputation")

- enforce_type:

  List of enforce_type specifications. Each element can be NULL or
  character vector of controls. Default: list(NULL)

- percent_effect:

  Vector of effect sizes to test. Default: seq(0.05, 0.20, 0.05)

- transform_outcome:

  Vector of outcome transformations. Default: c(NULL, "log")

- controls:

  List of control variable sets. Each element is a character vector or
  NULL. Default: list(NULL)

- models_to_run:

  Models to estimate (same as run_power_analysis). Default: c("cs",
  "imputation")

- n_sims:

  Number of simulations per specification (same as run_power_analysis).
  Default: 100

- min_year:

  Numeric or NULL. Minimum year to include in analysis. If NULL, no
  lower bound is applied. Use with max_year to filter data to specific
  time ranges (e.g., min_year = 1995, max_year = 2019). Default: NULL
  (uses all available years)

- max_year:

  Numeric or NULL. Maximum year to include in analysis. If NULL, no
  upper bound is applied. Use with min_year to filter data to specific
  time ranges (e.g., min_year = 1995, max_year = 2019). Default: NULL
  (uses all available years)

- parallel:

  Logical. Whether to parallelize across grid specifications. Default:
  FALSE (recommended). Setting to TRUE parallelizes the grid search, but
  requires n_specs × n_cores_per_sim total cores. For most use cases,
  parallelization at the Monte Carlo simulation level (via doParallel
  backend) is more efficient than grid-level parallelization.

- n_cores:

  Number of cores if parallel=TRUE. Default: NULL (uses all available -
  1)

- noise_spec:

  List. Noise engine configuration. Defaults to `list(engine = "none")`
  for deterministic benchmark. Use `list(engine = "iid")` for legacy
  stochastic behavior. See
  [`normalize_noise_spec`](https://zjelveh.github.io/staggeredpower/reference/normalize_noise_spec.md)
  for full options.

- design_resample:

  Character. Design-level resampling: "none" (default) or
  "cluster_bootstrap" (not yet implemented). Passed through to
  [`run_power_analysis`](https://zjelveh.github.io/staggeredpower/reference/run_power_analysis.md).

## Value

A list with three components:

- final_power:

  data.table with power analysis results for all specifications

- power_summary:

  data.table with summary statistics per specification

- specifications:

  data.table summarizing the grid of specifications run

## Details

When multiple outcomes are provided, the function loops over each
outcome, runs the full grid search, and combines results with an
'outcome' identifier column.

**Parallelization Strategy:**

This function supports two levels of parallelization, but typically only
ONE should be used:

1.  **Monte Carlo simulation level** (RECOMMENDED): Register a parallel
    backend with `doParallel` before calling this function. Each
    specification will run its Monte Carlo simulations in parallel. This
    is the most efficient approach for typical hardware.

2.  **Grid specification level**: Set `parallel = TRUE` to run different
    specifications in parallel. This requires
    `n_specs × n_cores_per_sim` total cores and is rarely practical.

**Recommended setup:**

    library(doParallel)
    cl <- makeCluster(detectCores() - 1)
    registerDoParallel(cl)

    # Run with parallel = FALSE (Monte Carlo sims will still be parallel)
    results <- run_power_grid(..., parallel = FALSE)

    stopCluster(cl)

See package README for detailed examples.

## Examples

``` r
# \donttest{
# Test multiple effect sizes using bundled dataset
# (requires 'did' or 'didimputation' package)
# results <- run_power_grid(
#   data_clean = nfs_panel,
#   unit_var = "state",
#   group_var = "treatment_year",
#   time_var = "year",
#   rel_pass_var = "rel_time",
#   treat_ind_var = "treated",
#   outcome = "assault_rate",
#   pta_type = "cs",
#   percent_effect = c(0.10, 0.15),
#   n_sims = 2,
#   parallel = FALSE
# )
# }
```
