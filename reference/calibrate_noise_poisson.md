# Calibrate noise parameters from Poisson model (log scale)

Calibrate noise parameters from Poisson model (log scale)

## Usage

``` r
calibrate_noise_poisson(
  mod_pois,
  df_untreated,
  working_outcome,
  pop_var,
  unit_col,
  time_col,
  noise_spec
)
```

## Arguments

- mod_pois:

  Fitted fepois model from untreated data

- df_untreated:

  data.table of untreated observations

- working_outcome:

  Character. Name of the working outcome column (count).

- pop_var:

  Character. Name of population variable.

- unit_col:

  Character. Unit column name.

- time_col:

  Character. Time column name.

- noise_spec:

  Normalized noise_spec list.

## Value

Calibration list with: scale, engine, sigma_log, rho, sigma_eta,
eta_pool, u_pool, obs_model
