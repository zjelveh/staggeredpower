# staggeredpower

`staggeredpower` is an R package designed for conducting power analysis in staggered difference-in-differences designs. It provides tools for enforcing parallel trends assumptions and simulating treatment effects to assess statistical power.

## Installation

You can install the development version of `staggeredpower` from GitHub:

```r
# install.packages("devtools")
devtools::install_github("zjelveh/staggeredpower")
```

## Usage
### Load Configuration
Load the power analysis configuration from a YAML file.

```r
library(staggeredpower)

config <- load_config("path/to/config.yaml")
```

## Run Power Analysis
Run power analysis with parallel trends.

```r
results <- run_power_analysis(
  data_clean = your_data,
  unit_var = "state",
  group_var = "year_passed",
  time_var = "year",
  rel_pass_var = "rel_pass",
  treat_ind_var = "law_pass",
  controls = c("control1", "control2"),
  outcome = "outcome_var",
  pta_type = "cs",
  enforce_type = "simple",
  percent_effect = 0.1,
  models_to_run = c("cs", "imputation", "twfe"),
  n_sims = 100
)
```

## Documentation
For detailed documentation, please refer to the function reference in the man directory.

## License
This package is licensed under the MIT License. See the LICENSE file for more details.

## Author
Zubin Jelveh - zjelveh@umd.edu