# Compute Wald Test on Pre-treatment Coefficients

Performs a joint Wald test that all pre-treatment coefficients are zero,
using the full variance-covariance matrix.

## Usage

``` r
compute_pretrend_wald_test(pre_coefs, pre_vcov)
```

## Arguments

- pre_coefs:

  Named numeric vector of pre-treatment coefficients

- pre_vcov:

  Variance-covariance matrix for pre_coefs

## Value

List with:

- p_value:

  P-value from chi-squared test

- wald_stat:

  Wald test statistic

- df:

  Degrees of freedom

- reject_at_05:

  Logical, TRUE if p \< 0.05

- warning:

  NULL if successful, otherwise explanation of failure

## Details

The Wald statistic is computed as W = beta' \* Sigma^(-1) \* beta, which
follows a chi-squared distribution with k degrees of freedom under the
null hypothesis that all coefficients are zero.
