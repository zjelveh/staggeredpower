# ETWFE Adapter Design: Wooldridge Poisson for Staggered DiD

## Overview

Add support for Wooldridge's Extended TWFE (etwfe) estimator to staggeredpower, including both linear and Poisson modes. This enables:
- Multiplicative parallel trends assumption (appropriate for rare events/counts)
- Automatic count ↔ rate transformation so all estimators can run in one call

## Background

### Why Wooldridge Poisson?

For rare outcomes like intimate partner homicide (IPH):

| Issue | Linear (CS/did2s) | Wooldridge Poisson |
|-------|-------------------|-------------------|
| PT assumption | Additive (level changes equal) | Multiplicative (growth rates equal) |
| Variance | Heteroskedastic for rates | Scales naturally with counts |
| Zeros | Treats Y=0 same as Y=10 | Handles through likelihood |
| Estimand | ATT in rate units | IRR (proportional effect) |

### References

- Wooldridge (2023) "Simple approaches to nonlinear difference-in-differences with panel data" *Econometrics Journal*
- `etwfe` R package (Grant McDermott)

## Design Decisions

### 1. New Parameters for `estimate_models()`

```r
estimate_models(
  ...
  outcome_type = NULL,    # "count" or "rate" - what's in outcome_var
  pop_var = NULL,         # Population column for transformation
  family = NULL,          # NULL (linear) or "poisson" - for etwfe
  ...
)
```

### 2. Transformation Logic

When mixing linear and Poisson estimators in one call:

| Adapter | If `outcome_type = "count"` | If `outcome_type = "rate"` |
|---------|----------------------------|---------------------------|
| cs, did2s, imputation | Transform: `rate = count/pop * 100000` | Use as-is |
| etwfe (linear) | Transform: `rate = count/pop * 100000` | Use as-is |
| etwfe (poisson) | Use count + `log(pop)` offset | Transform: `count = rate * pop / 100000`, then use + offset |

### 3. Backward Compatibility

- If `outcome_type = NULL`: assume rate, behave as before
- If `pop_var = NULL` and mixing estimator types: error with clear message
- Existing code continues to work unchanged

### 4. Result Standardization

All estimators return ATT on the **rate scale** for comparability:
- Linear estimators: ATT is already on rate scale
- Poisson: back-transform from log-rate to rate scale

```r
# Poisson returns δ on log-rate scale
# ATT_rate = mean_treated_rate * (1 - exp(-δ))
```

## Implementation

### Files to Create/Modify

1. **NEW: `R/adapter_etwfe.R`**
   - `adapter_etwfe()` function
   - Handles both `family = NULL` (linear) and `family = "poisson"`
   - Transformation logic for count ↔ rate

2. **MODIFY: `R/estimate_models.R`**
   - Add `outcome_type`, `pop_var`, `family` parameters
   - Pass to adapters

3. **MODIFY: `R/zzz.R`**
   - Register etwfe adapter on package load

4. **MODIFY: `R/adapter_cs.R`, `R/adapter_did2s.R`, `R/adapter_imputation.R`**
   - Add transformation logic when `outcome_type = "count"`

### Adapter Interface

```r
adapter_etwfe <- function() {
  fit_fn <- function(data,
                     outcome_var,
                     time_var,
                     id_var,
                     group_var,
                     controls = NULL,
                     cluster_var = NULL,
                     n_cores = NULL,
                     event_study = FALSE,
                     weightsname = NULL,
                     family = NULL,        # NEW
                     outcome_type = NULL,  # NEW
                     pop_var = NULL,       # NEW
                     ...) {

    # 1. Transform data if needed
    # 2. Call etwfe::etwfe() with appropriate family
    # 3. Call etwfe::emfx() to get ATT
    # 4. Back-transform results if Poisson
    # 5. Return standard format
  }

  extract_fn <- function(result) {
    # Return standard_estimate
  }

  estimator_adapter(
    name = "etwfe",
    fit_fn = fit_fn,
    extract_fn = extract_fn,
    requires = "etwfe"
  )
}
```

## Usage Examples

### Example 1: Rate data, linear estimators only (backward compatible)

```r
estimate_models(
  data = dat,
  outcome_var = "y_rate",
  models_to_run = c("cs", "did2s", "imputation")
)
```

### Example 2: Count data, all estimators

```r
estimate_models(
  data = dat,
  outcome_var = "y_count",
  outcome_type = "count",
  pop_var = "population",
  models_to_run = c("cs", "did2s", "imputation", "etwfe"),
  family = "poisson"  # Only affects etwfe
)
```

### Example 3: Rate data, include Poisson

```r
estimate_models(
  data = dat,
  outcome_var = "y_rate",
  outcome_type = "rate",
  pop_var = "population",  # Needed to back-calculate counts
  models_to_run = c("cs", "etwfe"),
  family = "poisson"
)
```

## Testing Plan

1. **Unit test**: etwfe adapter on simple simulated data
2. **Integration test**: Run all estimators on count data
3. **Integration test**: Run all estimators on rate data
4. **Comparison test**: Verify count+offset and rate give similar results
5. **Real data test**: Run on IPH data from base_rate_fallacy project

## Metadata Logging

Each result includes metadata about what was done:

```r
results$etwfe$metadata <- list(
  family = "poisson",
  outcome_type_input = "count",
  outcome_used = "count with log(population) offset",
  pt_assumption = "multiplicative (log-rate scale)",
  transformation = "none (count data provided)",
  estimand = "ATT on rate scale (back-transformed from IRR)"
)
```
