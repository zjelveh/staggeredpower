# Noise Engine Implementation Plan

**Date**: 2026-01-30
**Package**: staggeredpower
**Scope**: Replace inline iid noise with configurable noise engine (iid / AR(1) / AR(1)+common shocks)

---

## Overview

Currently, each `enforce_PTA_*()` function generates noise inline:
- **Imputation**: `rnorm(.N, 0, resid_sd)` — iid Normal, σ from `sigma(mod_fe)`
- **CS**: `rnorm(n, 0, resid_sd)` per (g,rp) cell — iid Normal, σ from control long-diff regression residuals
- **Poisson**: `rpois(.N, λ̂)` — Poisson sampling noise only (no log-scale residual process)

**Goal**: Factor noise generation into a shared engine with three modes:
1. `engine = "none"` — deterministic (ε ≡ 0)
2. `engine = "iid"` — reproduce current behavior exactly
3. `engine = "ar1"` — AR(1) idiosyncratic process per unit
4. `engine = "ar1_common"` — AR(1) idiosyncratic + common calendar-year shocks

Each mode calibrates from untreated/control residuals specific to the PTA method.

---

## Decisions (locked in)

| Decision | Choice |
|----------|--------|
| CS innovation pool | Global pool (across all cohorts g) by default; `noise_spec$cs_pool = "global" \| "cohort"` knob |
| CS iid backward compat | Exact per-(g,rp) cell replication: independent draw per cell |
| No-noise interface | `engine = "none"` inside `noise_spec` list; no separate boolean |
| Poisson no-noise | `engine = "none"` also disables `rpois` (returns expected λ̂) |
| Poisson obs_model | `obs_model = "deterministic" \| "poisson"` field; auto-set from engine |
| Skip Part C | Paired uniforms deferred to future work |

---

## Files to Create / Modify

| # | File | Action | Description |
|---|------|--------|-------------|
| 1 | `R/noise_engine.R` | **CREATE** | Core noise engine: `normalize_noise_spec()`, `calibrate_noise_imputation()`, `calibrate_noise_poisson()`, `calibrate_noise_cs()`, `draw_noise()`, `apply_noise_to_counterfactual()` |
| 2 | `R/enforce_pta.R` | **MODIFY** | Add `noise_spec` param to dispatcher + all 3 methods; refactor each to compute μ̂ first, then call engine |
| 3 | `R/power_analysis.R` | **MODIFY** | Pass `noise_spec` through to `enforce_PTA()` |
| 4 | `R/run_power_grid.R` | **MODIFY** | Pass `noise_spec` through |
| 5 | `tests/testthat/test-noise-engine.R` | **CREATE** | Unit tests for noise engine |
| 6 | `tests/testthat/test-enforce-pta-noise.R` | **CREATE** | Integration tests: backward compat + AR(1) + common shocks |
| 7 | `NAMESPACE` | **AUTO** | Will be updated by roxygen2 |

---

## Phase 1: Create `R/noise_engine.R`

### 1.1 `normalize_noise_spec(noise_spec)`

Validates and fills defaults:

```r
normalize_noise_spec <- function(noise_spec = NULL) {
  if (is.null(noise_spec)) noise_spec <- list()
  defaults <- list(
    engine = "iid",
    innovation = "normal",
    common_shock = FALSE,
    rho = NULL,             # NULL = estimate from data
    cs_pool = "global",     # "global" | "cohort"
    obs_model = NULL        # NULL = auto-set below
  )
  for (nm in names(defaults)) {
    if (is.null(noise_spec[[nm]])) noise_spec[[nm]] <- defaults[[nm]]
  }
  # Auto-set obs_model
  if (is.null(noise_spec$obs_model) || identical(noise_spec$obs_model, NULL)) {
    noise_spec$obs_model <- if (noise_spec$engine == "none") "deterministic" else "poisson"
  }
  # Validate
  stopifnot(noise_spec$engine %in% c("none", "iid", "ar1", "ar1_common"))
  stopifnot(noise_spec$innovation %in% c("normal", "empirical"))
  stopifnot(noise_spec$cs_pool %in% c("global", "cohort"))
  stopifnot(noise_spec$obs_model %in% c("deterministic", "poisson"))
  if (!is.null(noise_spec$rho)) {
    stopifnot(is.numeric(noise_spec$rho), noise_spec$rho >= 0, noise_spec$rho <= 0.99)
  }
  noise_spec
}
```

### 1.2 `calibrate_noise_imputation(mod_fe, df_untreated, unit_col, time_col, noise_spec)`

Returns a calibration object:

```r
# Returns list with:
#   $scale = "additive"
#   $sigma = sigma(mod_fe)                       # always computed
#   $rho = estimated or overridden               # if ar1/ar1_common
#   $sigma_eta = sd of AR(1) innovations         # if ar1/ar1_common
#   $eta_pool = vector of empirical innovations   # if innovation="empirical"
#   $u_pool = vector of time-FE innovations       # if common_shock=TRUE or engine="ar1_common"
```

**Steps** (for ar1/ar1_common):
1. Compute residuals: `df_untreated[, e_hat := resid(mod_fe)]`
2. Order by unit, time; create lag: `df_untreated[, e_lag := shift(e_hat, 1L), by = unit_col]`
3. Estimate ρ: `rho_hat <- coef(lm(e_hat ~ 0 + e_lag, data = df_untreated[!is.na(e_lag)]))`; clamp to [0, 0.95]. Override if `noise_spec$rho` is set.
4. Compute innovations: `eta = e_hat - rho * e_lag` (on complete cases)
5. If `innovation = "normal"`: `sigma_eta <- sd(eta, na.rm = TRUE)`
6. If `innovation = "empirical"`: `eta_pool <- eta[!is.na(eta)]`
7. If common_shock: extract time FE from model, compute year-to-year differences `Δγ_t`, store as `u_pool`

### 1.3 `calibrate_noise_poisson(mod_pois, df_untreated, pop_var, working_outcome, unit_col, time_col, noise_spec)`

Same structure but on log scale:

```r
# Returns list with:
#   $scale = "log"
#   $sigma_log = sd of log-scale residuals
#   $rho = estimated or overridden
#   $sigma_eta = sd of AR(1) innovations (log scale)
#   $eta_pool = vector of empirical innovations (log scale)
#   $u_pool = vector of common-shock innovations (log scale)
#   $obs_model = "deterministic" | "poisson"
```

**Steps** (for ar1/ar1_common):
1. Compute fitted λ̂ for untreated: merge unit_effect + time_effect + [controls/trends], then `λ̂ = exp(log_rate) * pop`
2. Log-scale residual: `e_hat = log(y + 0.5) - log(λ̂ + 0.5)` (c = 0.5 for zero counts)
3. Same AR(1) estimation as imputation but on `e_hat`
4. Common shock: `u_pool = df_untreated[, mean(e_hat), by = time_col]$V1`, then take differences if using innovations

### 1.4 `calibrate_noise_cs(df, unit_col, group_col, time_col, outcome_col, controls, noise_spec)`

CS-specific calibration using long-diff residuals:

```r
# Returns list with:
#   $scale = "additive"
#   $rho = estimated or overridden
#   $sigma_eta = sd of one-step innovations
#   $eta_pool = vector of one-step empirical innovations
#   $u_pool = vector of common-shock one-step innovations
#   $resid_sd_by_cell = list of per-(g,rp) resid_sd  # for iid backward compat
```

**Steps** (for ar1/ar1_common):
1. For each cohort g and each post-treatment year k ≥ g:
   - Identify control pool (not-yet-treated at k, within max_year)
   - Compute control long-diffs: Δ_s(g,k) = Y_{s,k} - Y_{s,g-1}
   - Fit same regression as current CS, get residuals: `resid_ld[s,k]`
2. Difference cumulative residuals to one-step: `r̃_{s,k} = resid_ld(s,k) - resid_ld(s,k-1)` with resid_ld(s,g-1) = 0
3. **If cs_pool = "global"**: Pool r̃ across all (g,s,k). **If cs_pool = "cohort"**: keep per-g pools.
4. Estimate ρ on r̃ by unit (same lm approach)
5. Common shock: `u_k = mean(r̃_{s,k})` across control states at each calendar year k
6. Store `eta_pool` and/or `sigma_eta`
7. **Also store per-(g,rp) `resid_sd`** for iid backward compatibility

### 1.5 `draw_noise(calib, units, times, seed = NULL)`

Dispatches based on `calib$engine` and `calib$scale`:

```r
# For engine = "none": return data.table with eps = 0
# For engine = "iid":
#   additive: rnorm(N, 0, calib$sigma)
#   CS: handled separately (per-cell draws)
# For engine = "ar1":
#   For each unit i, simulate v_{i,t} = rho * v_{i,t-1} + eta_{i,t}
#   eta from normal(0, sigma_eta) or sample(eta_pool)
#   eps = v_{i,t}
# For engine = "ar1_common":
#   Same as ar1 but also simulate u_t for each calendar year
#   u_t sampled from u_pool (or normal(0, sd(u_pool)))
#   eps = u_t + v_{i,t}
```

Returns `data.table(unit, time, eps)`.

### 1.6 `draw_noise_cs(calib, cohort_units, g, max_rp, seed = NULL)`

CS-specific draw that returns cumulated shocks:

```r
# For engine = "none": return 0 for all
# For engine = "iid": return per-(g,rp) independent draws from N(0, resid_sd_{g,rp})
# For engine = "ar1" / "ar1_common":
#   For each treated unit i in cohort g:
#     For k = g, g+1, ..., g+max_rp:
#       v_{i,k} = rho * v_{i,k-1} + eta_{i,k}  (eta from pool/normal)
#       u_k = sample from u_pool (if ar1_common)
#       one_step_{i,k} = u_k + v_{i,k}
#     eps_ld_{i,t} = cumsum(one_step_{i,g:t})
#   Return eps_ld for each (unit, rp)
```

Returns `data.table(unit, rp, eps_ld)` or matrix.

### 1.7 `apply_noise_to_counterfactual(df, eps_dt, scale, obs_model, pop_var, outcome_type, RATE_SCALE)`

Merges eps into df and applies:

```r
# For scale = "additive":
#   counterfactual := mu_hat + eps
# For scale = "log":
#   log_lambda_cf := mu_log_hat + eps
#   lambda_cf := exp(log_lambda_cf) * pop
#   If obs_model = "poisson": counterfactual_count := rpois(.N, lambda_cf)
#   If obs_model = "deterministic": counterfactual_count := round(lambda_cf)
#   Convert to rate if needed
```

---

## Phase 2: Refactor `enforce_PTA_imputation()`

### Changes (lines 136-299 of enforce_pta.R)

**Signature change**:
```r
enforce_PTA_imputation <- function(df, unit, group, time, outcome,
                                    controls = NULL, seed = NULL,
                                    trend_type = "common", trend_order = 1L,
                                    noise_spec = NULL)  # NEW
```

**Refactor steps**:

1. After `mod_fe <- feols(...)` (line 203): call `calibrate_noise_imputation(mod_fe, df_untreated, unit_col, time_col, noise_spec)`

2. **Compute μ̂ deterministically** for treated rows (replace all rnorm lines):
   - Lines 245-250 (common, with controls): change to `mu_hat := unit_effect + time_effect + control_effect`
   - Lines 248-250 (common, no controls): change to `mu_hat := unit_effect + time_effect`
   - Lines 277-279 (cohort_trend, with controls): change to `mu_hat := unit_effect + time_effect + .trend_effect + .control_effect`
   - Lines 282-284 (cohort_trend, no controls): change to `mu_hat := unit_effect + time_effect + .trend_effect`

3. **Draw and apply noise** after computing μ̂:
   - Call `draw_noise(calib, treated_units, treated_times, seed)`
   - Merge eps into df on (unit, time)
   - `df[treated == TRUE, counterfactual := mu_hat + eps]`

4. For untreated rows: `counterfactual := get(outcome_col)` (unchanged)

---

## Phase 3: Refactor `enforce_PTA_poisson()`

### Changes (lines 456-680 of enforce_pta.R)

**Signature change**:
```r
enforce_PTA_poisson <- function(df, unit, group, time, outcome,
                                 controls = NULL, seed = NULL,
                                 pop_var = NULL, outcome_type = "rate",
                                 trend_type = "common", trend_order = 1L,
                                 noise_spec = NULL)  # NEW
```

**Refactor steps**:

1. After `mod_pois <- fepois(...)` (line 570): compute λ̂ for untreated (for calibration), then call `calibrate_noise_poisson(...)`

2. **Compute deterministic μ̂ on log scale** for treated (rename `.log_rate` to `mu_log_hat`):
   - Lines 617-620: compute `.log_rate` as now → rename to `mu_log_hat`

3. **Draw noise on log scale**:
   - Call `draw_noise(calib, treated_units, treated_times, seed)`
   - Merge eps into df

4. **Apply noise**:
   - `log_lambda_cf := mu_log_hat + eps`
   - `lambda_cf := exp(log_lambda_cf) * pop`
   - If `obs_model = "poisson"`: `rpois(.N, lambda_cf)`
   - If `obs_model = "deterministic"`: `round(lambda_cf)` (or just `lambda_cf`)
   - Convert to rate if needed

---

## Phase 4: Refactor `enforce_PTA_CS()`

### Changes (lines 304-403 of enforce_pta.R)

This is the most complex refactor because the current function loops over (g, rp) cells.

**Signature change**:
```r
enforce_PTA_CS <- function(df, unit, group, time, outcome,
                            controls = NULL, seed = NULL,
                            noise_spec = NULL)  # NEW
```

**Refactor steps**:

### 4a. Pre-compute calibration (before the g×rp loop)

Call `calibrate_noise_cs(df, ...)` which:
- Loops over all (g, k) to collect control long-diff residuals
- Differences to one-step innovations
- Estimates ρ globally (or per-cohort if cs_pool = "cohort")
- Stores per-(g,rp) resid_sd for iid backward compat

### 4b. Pre-generate shock paths (if ar1/ar1_common)

For each treated unit, simulate the full one-step shock sequence before the main loop:

```r
if (engine %in% c("ar1", "ar1_common")) {
  shock_paths <- draw_noise_cs(calib, treated_units_by_cohort, ...)
  # Returns pre-computed eps_ld[unit, rp] for all treated (unit, rp) pairs
}
```

### 4c. Modify the inner loop (lines 364-398)

**Current** (line 392-393):
```r
counterfactuals = treated_pre[[outcome_col]] + predicted_changes +
    rnorm(length(predicted_changes), mean=0, sd=resid_sd)
```

**New**:
```r
mu_hat = treated_pre[[outcome_col]] + predicted_changes

if (engine == "none") {
  counterfactuals = mu_hat
} else if (engine == "iid") {
  counterfactuals = mu_hat + rnorm(length(mu_hat), 0, calib$resid_sd_by_cell[[paste(g, rp)]])
} else {
  # Look up pre-computed shock for each treated unit at this rp
  eps_ld = shock_paths[unit %in% treated_units & rp == rp_val, eps_ld]
  counterfactuals = mu_hat + eps_ld
}
```

### 4d. Performance improvement: keyed joins

Replace `merge(control_pre, control_post, by = unit_col)` with keyed data.table joins where possible. Pre-compute a lookup `Y_it` keyed by (unit, time) outside the loop.

---

## Phase 5: Update `enforce_PTA()` dispatcher

Add `noise_spec` parameter, normalize it, pass through:

```r
enforce_PTA <- function(df, unit, group, time, outcome,
                        controls = NULL,
                        method = c("imputation", "CS", "poisson"),
                        seed = NULL,
                        pop_var = NULL,
                        outcome_type = "rate",
                        trend_type = c("common", "cohort_trend"),
                        trend_order = 1L,
                        noise_spec = NULL) {
  # ... existing validation ...
  noise_spec <- normalize_noise_spec(noise_spec)

  if (method == "imputation") {
    enforce_PTA_imputation(df, unit, group, time, outcome, controls, seed,
                           trend_type, trend_order, noise_spec)
  } else if (method == "CS") {
    enforce_PTA_CS(df, unit, group, time, outcome, controls, seed, noise_spec)
  } else if (method == "poisson") {
    enforce_PTA_poisson(df, unit, group, time, outcome, controls, seed,
                        pop_var, outcome_type, trend_type, trend_order, noise_spec)
  }
}
```

---

## Phase 6: Update `run_power_analysis()` and `run_power_grid()`

Add `noise_spec = NULL` parameter to both. Pass through to all `enforce_PTA()` calls:

- `run_power_analysis()` line 103-115 (initial PTA check)
- `run_power_analysis()` line 125-137 (violation loop)
- `run_power_analysis()` line 214-226 (simulation loop)
- `run_power_grid()` — pass through to `run_power_analysis()`

---

## Phase 7: Tests

### 7.1 `test-noise-engine.R` — Unit tests for engine functions

1. **`normalize_noise_spec` defaults**: NULL → iid; engine="none" → obs_model="deterministic"
2. **`normalize_noise_spec` validation**: bad engine errors; rho out of range errors
3. **`calibrate_noise_imputation`**: on known data, verify ρ estimate is reasonable; sigma matches `sigma(mod_fe)`
4. **`draw_noise` with engine="none"**: returns all zeros
5. **`draw_noise` with engine="iid"**: returns draws with correct σ (check empirical SD ≈ target)
6. **`draw_noise` with engine="ar1"**: verify serial correlation in output (lag-1 autocorrelation > 0)
7. **`draw_noise` with engine="ar1_common"**: verify cross-unit correlation within same time period

### 7.2 `test-enforce-pta-noise.R` — Integration tests

1. **Backward compatibility (imputation)**: `noise_spec = list(engine="iid")` with same seed → same result as current code (no noise_spec). **NOTE**: This requires that the default noise_spec when NULL = "iid" to match current behavior.
2. **Backward compatibility (CS)**: Same test for CS method.
3. **Backward compatibility (Poisson)**: Same test for Poisson method.
4. **No-noise imputation**: `engine="none"` → counterfactual = exact μ̂ (verify no noise in output)
5. **No-noise Poisson**: `engine="none"` → counterfactual = expected count/rate (no rpois sampling)
6. **No-noise CS**: `engine="none"` → counterfactual = μ̂ (Y_{i,g-1} + predicted_changes)
7. **AR(1) sanity (imputation)**: On toy data, verify treated counterfactuals show serial correlation
8. **Poisson overdispersion**: With `engine="ar1"`, verify `var(count) > mean(count)` in treated counterfactuals
9. **Common shock cross-correlation**: With `engine="ar1_common"`, verify treated units in same calendar year have correlated counterfactuals

---

## Implementation Order

Execute in this order for incremental testability:

1. **Phase 1**: Create `noise_engine.R` with all functions
2. **Phase 7.1**: Write unit tests for noise engine (run and fix)
3. **Phase 2**: Refactor `enforce_PTA_imputation()` (simplest PTA)
4. **Phase 7.2 tests 1,4,7**: Backward compat + no-noise + AR(1) for imputation
5. **Phase 3**: Refactor `enforce_PTA_poisson()`
6. **Phase 7.2 tests 3,5,8**: Poisson backward compat + no-noise + overdispersion
7. **Phase 4**: Refactor `enforce_PTA_CS()` (most complex)
8. **Phase 7.2 tests 2,6,9**: CS backward compat + no-noise + common shocks
9. **Phase 5**: Update dispatcher
10. **Phase 6**: Update `run_power_analysis()` + `run_power_grid()`
11. **Run existing test suite** (`test-enforce-pta-trends.R`, `test-known-dgp.R`) to verify no regressions

---

## Backward Compatibility Contract

- `enforce_PTA(..., noise_spec = NULL)` behaves identically to current code (default engine = "iid")
- `run_power_analysis(..., noise_spec = NULL)` behaves identically to current code
- All existing tests pass without modification
- New `noise_spec` parameter is optional everywhere

---

## Open Questions (deferred)

- **Part C (paired uniforms)**: Deferred. Would allow low-MC-noise cross-PTA comparisons.
- **NegBin observation model**: Mentioned in plan but deferred. Poisson-lognormal (log-scale ε + Poisson draw) already provides overdispersion.
- **Block time bootstrap**: `noise_spec$block_time` mentioned but not implemented in v1.
