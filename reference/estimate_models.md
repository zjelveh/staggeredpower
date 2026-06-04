# Model-Agnostic DiD Estimation

Estimates difference-in-differences models using registered adapters.
Uses the adapter pattern for clean separation between common parameters
and package-specific translation.

## Usage

``` r
estimate_models(
  data,
  id_var,
  outcome_var,
  time_var,
  group_var,
  controls = NULL,
  models_to_run = c("cs", "imputation"),
  cluster_var = NULL,
  n_cores = NULL,
  event_study = FALSE,
  weightsname = NULL,
  outcome_type = NULL,
  pop_var = NULL,
  family = NULL,
  pretrend_test = FALSE,
  trend_type = c("common", "cohort_trend"),
  trend_order = 1L,
  ...
)
```

## Arguments

- data:

  data.table or data.frame. Panel data

- id_var:

  Character. Unit identifier variable name

- outcome_var:

  Character. Outcome variable name

- time_var:

  Character. Time variable name

- group_var:

  Character. Treatment group/cohort variable name

- controls:

  Character vector. Control variable names (default NULL)

- models_to_run:

  Character vector. Model names to estimate (must be registered)

- cluster_var:

  Character. Clustering variable (default: same as id_var)

- n_cores:

  Integer. Number of cores for parallel estimation (default:
  detectCores()-1)

- event_study:

  Logical. Compute event study estimates? (default FALSE)

- weightsname:

  Character. Column name for observation weights (default NULL for
  unweighted)

- outcome_type:

  Character. Type of outcome variable: "rate" or "count". If NULL
  (default), assumes "rate" for backward compatibility. When mixing
  linear (cs, did2s, imputation) and Poisson (etwfe_poisson) estimators,
  this enables automatic transformation so each estimator uses
  appropriate data format.

- pop_var:

  Character. Population variable name. Required when:

  - outcome_type = "count" and running linear estimators (to compute
    rate)

  - outcome_type = "rate" and running Poisson estimators (to compute
    count)

  - Using etwfe with family = "poisson" (for offset)

- family:

  Character. Distribution family for etwfe. NULL (default) for
  linear/Gaussian, "poisson" for Poisson regression with multiplicative
  parallel trends assumption. Only affects etwfe/etwfe_poisson adapters;
  ignored by other estimators.

- pretrend_test:

  Logical. Whether to compute pre-trend tests (default FALSE). When
  TRUE, adapters will compute Wald tests on pre-treatment coefficients.

- trend_type:

  Character. Type of time trend assumption: "common" (default) for
  standard parallel trends, or "unit_trend" for unit-specific linear
  trends. **EXPERIMENTAL**: The "unit_trend" option has not been
  extensively tested and may cause numerical issues (near-singular
  matrices) with many units. Use with caution. Only affects imputation
  and etwfe_poisson adapters.

- trend_order:

  Integer. Polynomial order for trends (default 1 = linear). Currently
  only linear trends (order=1) are supported for unit_trend.

- ...:

  Additional arguments passed to adapters

## Value

Named list of standard_estimate objects, one per model

## Examples

``` r
# \donttest{
# Estimate models using bundled dataset (requires 'did' or 'didimputation')
# if (requireNamespace("did", quietly = TRUE)) {
#   results <- estimate_models(
#     data = nfs_panel,
#     id_var = "state",
#     outcome_var = "assault_rate",
#     time_var = "year",
#     group_var = "treatment_year",
#     models_to_run = c("cs")
#   )
# }
# }
```
