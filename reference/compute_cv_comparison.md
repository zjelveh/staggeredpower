# Compare Stability of Additive vs Multiplicative Parallel Trends

Computes the coefficient of variation (CV) for both level differences
and ratios between early and late adopters in the pre-treatment period.
Lower CV indicates more stable (more plausible) parallel trends.

## Usage

``` r
compute_cv_comparison(data, outcome_var, time_var, group_var, id_var)
```

## Arguments

- data:

  Panel data

- outcome_var:

  Name of outcome variable

- time_var:

  Name of time variable

- group_var:

  Name of cohort/treatment timing variable

- id_var:

  Name of unit identifier

## Value

List with:

- ratio_cv:

  CV of ratios (multiplicative PT stability)

- diff_cv:

  CV of differences (additive PT stability)

- recommendation:

  "multiplicative" if ratio_cv \< diff_cv, else "additive"

## Details

This diagnostic helps researchers understand whether additive or
multiplicative parallel trends is more plausible for their data. It
compares early vs late adopters (split at median cohort) in the
pre-treatment period.
