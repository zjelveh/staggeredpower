# Enforce Parallel Trends Assumption

This function enforces the parallel trends assumption (PTA) by adjusting
post-treatment outcomes based on pre-treatment trends. It supports
multiple methods for enforcing the PTA and different control group
definitions.

## Usage

``` r
enforce_PTA(
  df,
  unit,
  group,
  time,
  outcome,
  controls = NULL,
  method = c("imputation", "CS", "poisson"),
  seed = NULL,
  pop_var = NULL,
  outcome_type = "rate",
  trend_type = c("common", "cohort_trend"),
  trend_order = 1L,
  noise_spec = NULL
)
```

## Arguments

- df:

  A data.frame containing panel data

- unit:

  Character. Name of unit identifier column (e.g., 'county_name')

- group:

  Character. Name of treatment group column (e.g., 'year_passed')

- time:

  Character. Name of time column (e.g., 'year')

- outcome:

  Character. Name of outcome column

- controls:

  Character vector. Names of control variables (default: NULL)

- method:

  Character. PTA enforcement method: 'imputation', 'CS', or 'poisson'
  (default: 'imputation')

- seed:

  Numeric. Random seed for reproducibility (default: NULL)

- pop_var:

  Character. For Poisson method, name of population variable (required
  for method='poisson')

- outcome_type:

  Character. For Poisson method, 'count' or 'rate'. Default 'rate'.

- trend_type:

  Character. Type of time trend: 'common' (default) uses standard TWFE
  with common time effects for all cohorts; 'cohort_trend' estimates
  cohort-specific polynomial time trends that can extrapolate to
  post-treatment periods.

- trend_order:

  Integer. For trend_type='cohort_trend', the polynomial order
  (1=linear, 2=quadratic, etc.). Default 1. Ignored when
  trend_type='common'.

- noise_spec:

  List. Noise engine configuration. NULL uses defaults (iid Normal, same
  as legacy behavior). See
  [`normalize_noise_spec`](https://zjelveh.github.io/staggeredpower/reference/normalize_noise_spec.md)
  for options:

  engine

  :   "none" (deterministic), "iid" (default, legacy), "ar1", or
      "ar1_common"

  innovation

  :   "normal" (default) or "empirical" (resample from residuals)

  common_shock

  :   TRUE/FALSE. Whether to include common calendar-year shocks.

  rho

  :   NULL (auto-estimate) or numeric in \\\[0, 0.99\]\\. AR(1)
      persistence parameter.

  cs_pool

  :   "global" (default) or "cohort". How to pool CS innovations.

  obs_model

  :   "deterministic" or "poisson" (default). For Poisson PTA only.

## Value

A data.frame with enforced parallel trends (includes 'counterfactual'
column)

## Details

Three methods are available:

- imputation:

  Uses two-way fixed effects on untreated observations to impute
  counterfactuals. Assumes additive parallel trends on level scale.

- CS:

  Uses Callaway-Sant'Anna style approach with not-yet-treated controls.
  Assumes additive parallel trends on level scale.

- poisson:

  Uses Poisson regression with unit and time fixed effects on untreated
  observations. Assumes multiplicative parallel trends on log-rate
  scale. This is appropriate for rare count outcomes (Wooldridge 2023).

## Examples

``` r
# \donttest{
# Linear approach (additive PT) using bundled dataset
pta_results <- enforce_PTA(
  df = nfs_panel,
  unit = "state",
  group = "treatment_year",
  time = "year",
  outcome = "assault_rate",
  method = "imputation"
)
#> NOTE: 1/0 fixed-effect singleton was removed (1 observation).
#> Warning: 969.233915 (type 'double') at RHS position 2 out-of-range(NA) or truncated (precision lost) when assigning to type 'integer' (column 10 named 'counterfactual')
# }
```
