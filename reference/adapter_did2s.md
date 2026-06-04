# Gardner Two-Stage Difference-in-Differences (2SDID) Adapter

Translates between staggeredpower's common interface and the `did2s`
package. This implements the Gardner (2022) two-stage DiD estimator,
which is robust to heterogeneous treatment effects and staggered
adoption.

## Usage

``` r
adapter_did2s()
```

## Value

An estimator_adapter for 2SDID estimation

## Examples

``` r
# \donttest{
adapter <- adapter_did2s()
# result <- adapter$fit(data = my_data, outcome_var = "y",
#   time_var = "year", id_var = "unit", group_var = "group")
# std_result <- adapter$extract(result)
# }
```
