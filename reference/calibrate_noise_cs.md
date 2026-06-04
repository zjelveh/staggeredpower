# Calibrate noise parameters for CS (Callaway-Sant'Anna) PTA

Computes one-step innovation residuals from control long-difference
regressions. Also stores per-(g,rp) residual SD for iid backward
compatibility.

## Usage

``` r
calibrate_noise_cs(
  df,
  unit_col,
  group_col,
  time_col,
  outcome_col,
  controls,
  noise_spec
)
```

## Arguments

- df:

  data.table with panel data

- unit_col:

  Character. Unit column name.

- group_col:

  Character. Group (cohort) column name.

- time_col:

  Character. Time column name.

- outcome_col:

  Character. Outcome column name.

- controls:

  Character vector. Control variable names (or NULL).

- noise_spec:

  Normalized noise_spec list.

## Value

Calibration list with: scale, engine, rho, sigma_eta, eta_pool, u_pool,
resid_sd_by_cell (for iid backward compat)
