# No-Noise Deterministic Benchmark Mode

**Date:** 2026-01-30
**Status:** In Progress

---

## Goal

Make `engine="none"` the default path in `run_power_analysis()` and `run_power_grid()`, treating it as a deterministic detectability benchmark. Add guards, metadata, and regression tests to make the mode production-ready.

## Design Decisions

1. **Per-function defaults, not global**: `normalize_noise_spec(NULL)` keeps `engine="iid"` as the ambient default. Only `run_power_analysis()` and `run_power_grid()` default to `list(engine="none")`. This prevents surprise behavior changes for standalone `enforce_PTA()` callers.

2. **n_sims guard**: When `engine="none"` and `n_sims > 1`, auto-set `n_sims <- 1` with `message()` (not `warning()` or `stop()`). Repeated sims are identical under deterministic mode.

3. **One-pass PTA violation loop**: Under `engine="none"`, call `enforce_PTA()` once, check violations once, drop units once. No `while(!continue)` loop — rerunning produces identical results.

4. **design_resample stub**: Add `design_resample = "none"` parameter to signatures. `"cluster_bootstrap"` accepted syntactically but `stop("not yet implemented")`. Mentioned in the `n_sims` guard message.

5. **Output metadata**: Add `noise_engine`, `obs_model`, `design_resample` columns to simulation result rows.

## Files to Modify

| # | File | Change |
|---|------|--------|
| 1 | `R/power_analysis.R` | Default `noise_spec=list(engine="none")`, `design_resample` stub, `n_sims` guard, one-pass violation loop, output metadata |
| 2 | `R/run_power_grid.R` | Default `noise_spec=list(engine="none")`, `design_resample` stub, passthrough |
| 3 | `R/noise_engine.R` | No change (global default stays `iid`) |
| 4 | `R/enforce_pta.R` | No change (already handles `engine="none"`) |
| 5 | `tests/testthat/test-no-noise-determinism.R` | NEW — 5 regression tests |

## Implementation Details

### run_power_analysis() Changes

```r
run_power_analysis <- function(...,
                               noise_spec = list(engine = "none"),  # NEW default
                               design_resample = "none",            # NEW stub
                               ...) {
  noise_spec <- normalize_noise_spec(noise_spec)

  # Validate design_resample
  design_resample <- match.arg(design_resample, c("none", "cluster_bootstrap"))
  if (design_resample == "cluster_bootstrap") {
    stop("design_resample='cluster_bootstrap' is not yet implemented. ",
         "Use engine='iid' or engine='ar1' for stochastic power analysis.")
  }

  # Deterministic guard
  if (noise_spec$engine == "none" && n_sims > 1) {
    n_sims <- 1L
    message("engine='none' produces deterministic data; setting n_sims=1. ",
            "To get a rejection probability without outcome noise, use ",
            "design_resample='cluster_bootstrap'.")
  }
```

### One-Pass Violation Loop

```r
if (is.null(transform_outcome)) {
  pta_enforced_orig[, bound_error := ifelse(counterfactual < 0, 1, 0)]
  pta_enforced_orig[bound_error == 1, counterfactual := 0]
  pta_enforced_orig[, na_error := ifelse(is.na(counterfactual), 1, 0)]

  if (noise_spec$engine == "none") {
    # Deterministic: single-pass (rerunning won't change anything)
    max_year_check <- if (!is.null(max_year)) max_year else max(pta_enforced_orig[[time_var]])
    pta_violations <- copy(pta_enforced_orig[bound_error == 1 |
                            (na_error == 1 & get(time_var) < max_year_check)])
    if (nrow(pta_violations) > 0) {
      data_clean_copy <- data_clean_copy[!get(unit_var) %in% pta_violations[[unit_var]]]
      units_to_drop <- sort(unique(pta_violations[[unit_var]]))
    }
    pta_enforced <- pta_enforced_orig
  } else {
    # Stochastic: existing while loop
    while (!continue) { ... }
  }
}
```

### Output Metadata

```r
data.table(
  model = model,
  # ... existing columns ...
  noise_engine = noise_spec$engine,
  obs_model = noise_spec$obs_model,
  design_resample = design_resample,
  # ... rest ...
)
```

## Regression Tests

1. **Seed invariance**: `engine="none"` with seed=42 and seed=999 → identical counterfactuals (all 3 methods)
2. **Single-sim enforcement**: `engine="none"` + `n_sims=100` → message emitted, only 1 sim runs
3. **One-pass violations**: data with known violations → units dropped, no re-enforcement
4. **iid backward compat**: `engine="iid"` + fixed seed → same draws as legacy
5. **Output metadata**: result includes `noise_engine`, `obs_model`, `design_resample` columns

## Backward Compatibility

- `enforce_PTA(noise_spec = NULL)` → defaults to `iid` (unchanged)
- `run_power_analysis(noise_spec = NULL)` → defaults to `none` (NEW)
- `run_power_analysis(noise_spec = list(engine = "iid"))` → legacy stochastic behavior (unchanged)
- All existing tests continue to pass
