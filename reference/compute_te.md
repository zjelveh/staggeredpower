# Compute Treatment Effects

Compute Treatment Effects

## Usage

``` r
compute_te(
  df,
  pta_type,
  enforce_type,
  group_var,
  time_var,
  rel_pass_var,
  outcome
)
```

## Arguments

- df:

  Dataset

- pta_type:

  Which pta assumption ("cs" or "imputation")

- enforce_type:

  Method for enforcing parallel trends

- group_var:

  Group variable name

- time_var:

  Time variable name

- rel_pass_var:

  Relative passage variable name

- outcome:

  Outcome variable name
