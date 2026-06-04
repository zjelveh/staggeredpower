# NFS Law Adoption Panel Dataset

A balanced panel dataset of U.S. states with staggered adoption of
non-fatal strangulation (NFS) assault laws. Suitable for demonstrating
power analysis in staggered difference-in-differences designs.

## Usage

``` r
nfs_panel
```

## Format

A data frame with the following columns:

- state:

  State name

- year:

  Calendar year

- treatment_year:

  Year the state adopted NFS law (NA if never-treated)

- treated:

  Treatment indicator: 1 if post-treatment, 0 otherwise

- rel_time:

  Relative time to treatment (year - treatment_year)

- assault_rate:

  Aggravated assault rate

- clearance_rate:

  Arrest clearance rate for aggravated assaults

## Source

Derived from NIBRS (National Incident-Based Reporting System) data on
aggravated assault clearance rates across U.S. states.
