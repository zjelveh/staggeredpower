# ETWFE Poisson Adapter (Saturated Wooldridge 2023)

Uses fixest::fepois() with saturated cohort x time treatment indicators
and observation-proportional aggregation weights for the overall ATT.

## Usage

``` r
adapter_etwfe_poisson_glm()
```

## Value

An estimator_adapter registered as "etwfe_poisson"
