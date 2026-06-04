# Calibrate noise parameters from imputation (TWFE) model

Extracts residual structure from untreated data to calibrate the noise
engine. Works on additive (level) scale.

## Usage

``` r
calibrate_noise_imputation(
  mod_fe,
  df_untreated,
  unit_col,
  time_col,
  noise_spec
)
```

## Arguments

- mod_fe:

  Fitted feols model from untreated data

- df_untreated:

  data.table of untreated observations (must include unit and time
  columns)

- unit_col:

  Character. Unit column name.

- time_col:

  Character. Time column name.

- noise_spec:

  Normalized noise_spec list.

## Value

Calibration list with: scale, engine, sigma, rho, sigma_eta, eta_pool,
u_pool
