# Draw noise for CS PTA (cumulated one-step shocks)

For CS, noise at horizon rp is the cumulative sum of one-step shocks
from treatment onset through rp. This preserves the long-difference
structure.

## Usage

``` r
draw_noise_cs(calib, treated_units_by_cohort, max_rp_by_cohort, seed = NULL)
```

## Arguments

- calib:

  Calibration object from calibrate_noise_cs()

- treated_units_by_cohort:

  Named list: cohort -\> vector of treated unit IDs

- max_rp_by_cohort:

  Named list: cohort -\> maximum relative period

- seed:

  Optional random seed

## Value

data.table with columns: unit, cohort, rp, eps_ld (cumulated shock)
