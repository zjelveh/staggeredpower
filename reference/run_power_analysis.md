# Run Power Analysis with Parallel Trends

Run Power Analysis with Parallel Trends

## Usage

``` r
run_power_analysis(
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
  models_to_run = c("cs", "imputation"),
  n_sims = 100,
  min_year = NULL,
  max_year = NULL,
  pretrend_test = FALSE,
  outcome_type = NULL,
  pop_var = NULL,
  total_count_var = NULL,
  family = NULL,
  trend_type = "common",
  trend_order = 1L,
  noise_spec = list(engine = "none"),
  design_resample = "none",
  allow_unbalanced_panel = FALSE,
  est_method = "dr",
  base_period = "varying"
)
```

## Arguments

- data_clean:

  Clean panel dataset

- unit_var:

  Level of analysis (state or county)

- group_var:

  Character. Name of the treatment group/cohort column

- time_var:

  Character. Name of the time period column

- rel_pass_var:

  Character. Name of the relative time to treatment column

- treat_ind_var:

  Character. Name of the treatment indicator column

- controls:

  list of controls

- outcome:

  Outcome variable

- transform_outcome:

  Character. Outcome transformation: NULL (multiplicative effects,
  default), "log" (log transformation), or "ihs" (inverse hyperbolic
  sine). IHS recommended for outcomes with zeros (e.g., crime counts).
  When transformed, PTA violations are not checked.

- pta_type:

  Which pta assumption ("cs" or "imputation")

- enforce_type:

  Method for enforcing parallel trends

- percent_effect:

  Effect size to simulate

- models_to_run:

  Character vector. Models to estimate (default c('cs', 'imputation'))

- n_sims:

  Number of simulations

- min_year:

  Numeric. Minimum year to include (optional, default NULL = no minimum)

- max_year:

  Numeric. Maximum year to include (optional, default NULL = no maximum)

- pretrend_test:

  Logical. Whether to compute pre-trend tests (default FALSE)

- outcome_type:

  Character. Type of outcome: "rate" or "count" (default NULL = rate)

- pop_var:

  Character. Population variable name for Poisson models (default NULL)

- total_count_var:

  Character. Total count variable for binomial models (default NULL)

- family:

  Character. Distribution family for etwfe: NULL (linear) or "poisson"
  (default NULL)

- trend_type:

  Character. Type of time trend for PTA enforcement: 'common' (default)
  uses standard TWFE with common time effects for all cohorts;
  'cohort_trend' estimates cohort-specific polynomial time trends that
  can extrapolate to post-treatment periods. Ignored for pta_type='cs'.

- trend_order:

  Integer. For trend_type='cohort_trend', the polynomial order
  (1=linear, 2=quadratic, etc.). Default 1. Ignored when
  trend_type='common'.

- noise_spec:

  List. Noise engine configuration. Defaults to `list(engine = "none")`
  for deterministic benchmark. Use `list(engine = "iid")` for legacy
  stochastic behavior. See
  [`normalize_noise_spec`](https://zjelveh.github.io/staggeredpower/reference/normalize_noise_spec.md)
  for full options.

- design_resample:

  Character. Design-level resampling: "none" (default) or
  "cluster_bootstrap" (not yet implemented). When engine="none", this is
  the only way to get a rejection probability without outcome noise.

- allow_unbalanced_panel:

  Logical. Allow unbalanced panels (default FALSE)

- est_method:

  Character. Estimation method for CS estimator (default "dr")

- base_period:

  Character. Base period for CS estimator (default "varying")
