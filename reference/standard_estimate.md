# Standard Estimate Format

All adapters must return results in this format

## Usage

``` r
standard_estimate(
  att,
  se,
  model_name,
  event_study = NULL,
  raw_result = NULL,
  metadata = list()
)
```

## Arguments

- att:

  Numeric. Average treatment effect on treated

- se:

  Numeric. Standard error

- model_name:

  Character. Model identifier

- event_study:

  Optional. Event study results (data.table)

- raw_result:

  Optional. Original package output for debugging

- metadata:

  Optional. Additional model-specific info

## Value

A standard_estimate object
