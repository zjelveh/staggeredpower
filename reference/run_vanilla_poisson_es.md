# Run Vanilla Poisson Event Study for Pre-trend Testing

Runs a standard Poisson fixed-effects event study to extract
pre-treatment coefficients for testing multiplicative parallel trends.
This is used for ETWFE Poisson models, which don't produce pre-treatment
coefficients by design.

## Usage

``` r
run_vanilla_poisson_es(
  data,
  outcome_var,
  time_var,
  id_var,
  group_var,
  cluster_var,
  ref_period = -1,
  min_event = -10,
  max_event = 10
)
```

## Arguments

- data:

  Data frame with panel data

- outcome_var:

  Name of outcome variable (count)

- time_var:

  Name of time variable

- id_var:

  Name of unit identifier

- group_var:

  Name of cohort/treatment timing variable

- cluster_var:

  Name of clustering variable for SEs

- ref_period:

  Reference period for event study (default -1)

- min_event:

  Minimum event time to include (default -10)

- max_event:

  Maximum event time to include (default 10)

## Value

List with:

- pre_coefs:

  Named vector of pre-treatment coefficients

- pre_vcov:

  Variance-covariance matrix for pre_coefs

- model:

  The fitted fixest model object

- method:

  String identifying the method used
