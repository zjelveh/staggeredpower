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
