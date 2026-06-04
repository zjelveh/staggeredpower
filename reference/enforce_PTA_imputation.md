# Generate counterfactual outcomes using imputation approach

This function implements the imputation approach for
difference-in-differences with staggered adoption. It estimates fixed
effects using only untreated observations and generates counterfactual
outcomes for treated observations.

## Usage

``` r
enforce_PTA_imputation(
  df,
  unit,
  group,
  time,
  outcome,
  controls = NULL,
  seed = NULL,
  trend_type = "common",
  trend_order = 1L,
  noise_spec = NULL
)
```

## Arguments

- df:

  A data.table containing the panel data

- unit:

  Character string specifying the column name for unit identifiers

- group:

  Character string specifying the column name for treatment group/cohort

- time:

  Character string specifying the column name for time periods

- outcome:

  Character string specifying the column name for the outcome variable

- controls:

  Character string specifying whether to include controls ("controls")
  or use only fixed effects ("none")

## Value

A data.table with an additional column 'counterfactual' containing:

- Actual outcomes for untreated observations

- Imputed counterfactual outcomes for treated observations

## Details

The function follows these steps:

1.  Splits data into treated and untreated observations

2.  Estimates fixed effects model on untreated observations only

3.  Uses these estimates to impute counterfactuals for treated
    observations

The imputation draws from a normal distribution centered at the
predicted mean with variance equal to the residual variance from the
first-stage regression.
