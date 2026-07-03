# staggeredpower 0.2.2

* **CS power DGP harmonized with the CS estimator.** `enforce_PTA_CS()` previously
  re-derived the Callaway--Sant'Anna control counterfactual with a hand-rolled
  long-difference regression that only matched `did::att_gt()` on complete-grid
  (balanced) panels. On unbalanced panels the two diverged, leaving a non-zero
  CS/CS "null bias" in the power analysis (the estimator did not recover ~0 when
  applied to a no-effect DGP). The DGP now reads did's own control-side straight
  off a single `att_gt()` fit (`delta_control(g,t) = treated_change - ATT(g,t)`,
  in which the realized treated outcome cancels), so the DGP inherits exactly the
  estimator's control group and estimand. Null recovery is now ~0 on balanced
  panels (machine precision) and near-zero on ragged ones. Adds `control_group`,
  `est_method`, `base_period`, and `allow_unbalanced_panel` arguments to
  `enforce_PTA()` (CS method) so the DGP is configured identically to the estimator.
* Fixed a latent bug in `enforce_PTA_CS()`: group/time filters used data.table NSE
  (`get(group_col) == g`), which silently matched all rows when the caller's group
  column was literally named `g`. Filters now use base-R column extraction.
* Fixed a length-mismatch crash in `enforce_PTA_CS()` on panels where a treated
  cohort's membership differed between the base year and a post period; the
  counterfactual is now assigned by a per-unit keyed join.
* Fixed `enforce_PTA_CS()` for a cohort with no observed post-period (max relative
  time < 0, e.g. a unit that adopts just after the last observed year): the
  `0:max_rel_pass` loop previously descended into negative relative time and
  overwrote pre-treatment outcomes (or errored on `0:-Inf`). Such cohorts are now
  skipped.
* `enforce_PTA_CS()` now warns loudly when `did::att_gt()` estimates no usable
  (g, t) cells (e.g. a single treated cohort with no controls) instead of silently
  returning the observed outcomes as the "counterfactual."
* Guarded a base-R group filter in the CS ar1 noise path against never-treated
  (NA-group) rows leaking `NA` unit ids into the per-cohort shock lookup.
* The CS control-counterfactual `did::att_gt()` fit is now memoized (keyed on the
  data values + estimator settings), since it is invariant across simulations,
  effect sizes and noise configurations. This avoids repeating the identical fit
  tens of thousands of times over a power grid. Adds `digest` to Imports.

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
