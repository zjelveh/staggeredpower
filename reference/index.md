# Package index

## Core Functions

Main entry points for power analysis

- [`run_power_analysis()`](https://zjelveh.github.io/staggeredpower/reference/run_power_analysis.md)
  : Run Power Analysis with Parallel Trends
- [`run_power_grid()`](https://zjelveh.github.io/staggeredpower/reference/run_power_grid.md)
  : Run Power Analysis Grid Search
- [`estimate_models()`](https://zjelveh.github.io/staggeredpower/reference/estimate_models.md)
  : Model-Agnostic DiD Estimation

## Estimator Adapters

Pluggable backends for different DiD estimators

- [`adapter_cs()`](https://zjelveh.github.io/staggeredpower/reference/adapter_cs.md)
  : Callaway-Sant'Anna Adapter
- [`adapter_did2s()`](https://zjelveh.github.io/staggeredpower/reference/adapter_did2s.md)
  : Gardner Two-Stage Difference-in-Differences (2SDID) Adapter
- [`adapter_imputation()`](https://zjelveh.github.io/staggeredpower/reference/adapter_imputation.md)
  : Borusyak-Jaravel-Spiess Imputation Adapter
- [`adapter_etwfe_poisson_glm()`](https://zjelveh.github.io/staggeredpower/reference/adapter_etwfe_poisson_glm.md)
  : ETWFE Poisson Adapter (Saturated Wooldridge 2023)

## Diagnostics

Pre-trend testing and PTA enforcement

- [`compute_pretrend_wald_test()`](https://zjelveh.github.io/staggeredpower/reference/compute_pretrend_wald_test.md)
  : Compute Wald Test on Pre-treatment Coefficients
- [`enforce_PTA()`](https://zjelveh.github.io/staggeredpower/reference/enforce_PTA.md)
  : Enforce Parallel Trends Assumption

## Utilities

Configuration, storage, and adapter management

- [`load_config()`](https://zjelveh.github.io/staggeredpower/reference/load_config.md)
  : Load Power Analysis Configuration
- [`store_results()`](https://zjelveh.github.io/staggeredpower/reference/store_results.md)
  : Store Power Analysis Results
- [`register_adapter()`](https://zjelveh.github.io/staggeredpower/reference/register_adapter.md)
  : Register an Adapter
- [`get_adapter()`](https://zjelveh.github.io/staggeredpower/reference/get_adapter.md)
  : Get Registered Adapter
- [`list_adapters()`](https://zjelveh.github.io/staggeredpower/reference/list_adapters.md)
  : List All Registered Adapters

## Data

- [`nfs_panel`](https://zjelveh.github.io/staggeredpower/reference/nfs_panel.md)
  : NFS Law Adoption Panel Dataset
