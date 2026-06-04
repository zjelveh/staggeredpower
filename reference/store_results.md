# Store Power Analysis Results

Store Power Analysis Results

## Usage

``` r
store_results(
  results,
  data,
  aggregate_results,
  event_study_results,
  pta_type,
  enforce_type,
  analysis_level,
  outcome,
  use_controls,
  drop_add_states,
  result_type,
  transform_outcome,
  treat_col = "law_pass",
  group_col = "year_passed"
)
```

## Arguments

- results:

  Analysis results

- data:

  Original dataset

- aggregate_results:

  Aggregated results

- event_study_results:

  Event study results

- pta_type:

  Pta type

- enforce_type:

  Enforce type

- analysis_level:

  Analysis level

- outcome:

  Outcome variable

- use_controls:

  Control usage

- drop_add_states:

  State dropping

- result_type:

  Result type

- transform_outcome:

  Transform outcome variable

- treat_col:

  Name of the treatment indicator column in `data`. Defaults to
  `"law_pass"` for backward compatibility.

- group_col:

  Name of the treatment timing (cohort/group) column in `data`. Defaults
  to `"year_passed"` for backward compatibility.
