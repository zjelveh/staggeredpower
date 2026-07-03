# staggeredpower 0.2.1

* The CS adapter's unbalanced-panel warning now uses a complete-grid test
  (distinct `(id, time)` cells vs `n_ids * n_times`) instead of comparing
  per-unit period counts. The count test missed same-length but
  different-coverage panels (e.g. one unit observed 2000-2010 and another
  2005-2015); the grid test matches the criterion `did` actually applies.

# staggeredpower 0.2.0

* `estimate_models()` now exposes `allow_unbalanced_panel` (default `FALSE`,
  matching `did`) as a first-class argument, forwarded to the CS/etwfe adapters.
* The CS adapter **warns** when the panel is unbalanced and
  `allow_unbalanced_panel = FALSE`, because `did` silently coerces to a balanced
  (complete-unit) panel in that case, changing the estimand.
* `estimate_models()` exposes event-study aggregation controls `balance_e`,
  `min_e`, `max_e`, forwarded to `did::aggte(type = "dynamic")` for the CS
  adapter. Defaults (`NULL`) preserve prior behavior.
* New exported helper `build_event_study_sample()` constructs a
  balanced-composition event-study sample via a unit-level presence filter over
  a displayed window, for use with any estimator (CS or imputation/did2s).

# staggeredpower 0.1.0

* Initial CRAN release.
* Core functions: `run_power_analysis()`, `run_power_grid()`, `estimate_models()`.
* Pluggable estimator adapters: Callaway-Sant'Anna (`adapter_cs`), Gardner 2SDID (`adapter_did2s`),
  Borusyak-Jaravel-Spiess imputation (`adapter_imputation`), ETWFE Poisson (`adapter_etwfe_poisson_glm`).
* Parallel trends enforcement via `enforce_PTA()` with imputation, CS, and Poisson methods.
* Configurable noise engines: `none` (deterministic), `iid`, `ar1`, `ar1_common`, `ar1_anchored`.
* Pre-trend diagnostic testing with `compute_pretrend_wald_test()`.
* Custom adapter registration via `register_adapter()`.
* YAML-based configuration with `load_config()`.
* Optional parallel execution via `foreach`/`doParallel` with automatic `lapply` fallback.
