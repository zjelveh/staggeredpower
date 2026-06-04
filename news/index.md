# Changelog

## staggeredpower 0.1.0

- Initial CRAN release.
- Core functions:
  [`run_power_analysis()`](https://zjelveh.github.io/staggeredpower/reference/run_power_analysis.md),
  [`run_power_grid()`](https://zjelveh.github.io/staggeredpower/reference/run_power_grid.md),
  [`estimate_models()`](https://zjelveh.github.io/staggeredpower/reference/estimate_models.md).
- Pluggable estimator adapters: Callaway-Sant’Anna (`adapter_cs`),
  Gardner 2SDID (`adapter_did2s`), Borusyak-Jaravel-Spiess imputation
  (`adapter_imputation`), ETWFE Poisson (`adapter_etwfe_poisson_glm`).
- Parallel trends enforcement via
  [`enforce_PTA()`](https://zjelveh.github.io/staggeredpower/reference/enforce_PTA.md)
  with imputation, CS, and Poisson methods.
- Configurable noise engines: `none` (deterministic), `iid`, `ar1`,
  `ar1_common`, `ar1_anchored`.
- Pre-trend diagnostic testing with
  [`compute_pretrend_wald_test()`](https://zjelveh.github.io/staggeredpower/reference/compute_pretrend_wald_test.md).
- Custom adapter registration via
  [`register_adapter()`](https://zjelveh.github.io/staggeredpower/reference/register_adapter.md).
- YAML-based configuration with
  [`load_config()`](https://zjelveh.github.io/staggeredpower/reference/load_config.md).
- Optional parallel execution via `foreach`/`doParallel` with automatic
  `lapply` fallback.
