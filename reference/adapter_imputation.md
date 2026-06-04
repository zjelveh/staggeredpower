# Borusyak-Jaravel-Spiess Imputation Adapter

Translates between staggeredpower's common interface and the
`didimputation` package. Assumes additive parallel trends (level scale).

## Usage

``` r
adapter_imputation()
```

## Value

An estimator_adapter for imputation-based estimation

## Examples

``` r
# \donttest{
adapter <- adapter_imputation()
# result <- adapter$fit(data = my_data, outcome_var = "y",
#   time_var = "year", id_var = "unit", group_var = "group")
# std_result <- adapter$extract(result)
# }
```
