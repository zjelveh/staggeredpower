# Callaway-Sant'Anna Adapter

Translates between staggeredpower's common interface and the `did`
package

## Usage

``` r
adapter_cs()
```

## Value

An estimator_adapter for Callaway-Sant'Anna estimation

## Examples

``` r
# \donttest{
adapter <- adapter_cs()
# result <- adapter$fit(data = my_data, outcome_var = "y",
#   time_var = "year", id_var = "unit", group_var = "group")
# std_result <- adapter$extract(result)
# }
```
