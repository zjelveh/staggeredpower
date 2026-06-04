# Draw noise for imputation/Poisson PTA (level or log scale)

Draw noise for imputation/Poisson PTA (level or log scale)

## Usage

``` r
draw_noise(calib, units, times, seed = NULL)
```

## Arguments

- calib:

  Calibration object from calibrate_noise_imputation() or
  calibrate_noise_poisson()

- units:

  Vector of unit IDs for treated observations

- times:

  Vector of time values for treated observations

- seed:

  Optional random seed

## Value

data.table with columns: unit, time, eps
