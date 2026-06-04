# Generate counterfactual outcomes using Poisson approach (multiplicative PT)

This function implements a Poisson-based approach for
difference-in-differences with staggered adoption. It estimates a
Poisson fixed effects model using only untreated observations and
generates counterfactual outcomes for treated observations on the
log-rate scale, then back-transforms to counts or rates.

This implements the multiplicative parallel trends assumption
(Wooldridge 2023): \\E\[Y_t(0)\] / E\[Y\_{t-1}(0)\]\\ is constant across
treatment groups.

## Usage

``` r
enforce_PTA_poisson(
  df,
  unit,
  group,
  time,
  outcome,
  controls = NULL,
  seed = NULL,
  pop_var = NULL,
  outcome_type = "rate",
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

  Character vector. Names of control variables (default: NULL)

- seed:

  Numeric. Random seed for reproducibility (default: NULL)

- pop_var:

  Character. Name of population variable (required)

- outcome_type:

  Character. 'count' or 'rate' (default: 'rate')

## Value

A data.table with an additional column 'counterfactual' containing:

- Actual outcomes for untreated observations

- Imputed counterfactual outcomes for treated observations

## Details

The function follows these steps:

1.  Converts rates to counts if needed (count = rate \* pop / 100000)

2.  Splits data into treated and untreated observations

3.  Estimates Poisson fixed effects model on untreated observations:
    \\E\[count \| untreated\] = exp(alpha_i + gamma_t + X\*beta)\\ with
    log(pop) offset

4.  Uses these estimates to predict counterfactual log-rates for treated
    observations

5.  Adds Poisson noise (using rpois) to generate stochastic
    counterfactuals

6.  Converts back to rate scale if needed
