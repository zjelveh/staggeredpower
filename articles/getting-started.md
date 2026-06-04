# Getting Started with staggeredpower

## Why power analysis for staggered DiD?

Staggered difference-in-differences (DiD) designs are common in policy
evaluation, where different units adopt a treatment at different times.
Modern heterogeneity-robust estimators (Callaway & Sant’Anna 2021,
Borusyak et al. 2024, Gardner 2022) address bias from treatment effect
heterogeneity, but their statistical power properties are less well
understood.

`staggeredpower` fills this gap by simulating the data-generating
process under controlled conditions: it enforces parallel trends on your
actual data, injects treatment effects of a specified size, and
estimates DiD models to compute rejection rates. This lets you answer:
“Given my panel structure, how large must the effect be for my estimator
to detect it?”

## Quick example

``` r
library(staggeredpower)
data(nfs_panel)
str(nfs_panel)
#> 'data.frame':    283 obs. of  7 variables:
#>  $ state         : chr  "colorado" "connecticut" "idaho" "iowa" ...
#>  $ year          : int  2003 2003 2003 2003 2003 2003 2003 2003 2003 2003 ...
#>  $ treatment_year: int  2016 2007 2005 2012 2014 2012 2004 2007 2023 2023 ...
#>  $ treated       : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ rel_time      : int  -13 -4 -2 -9 -11 -9 -1 -4 -20 -20 ...
#>  $ assault_rate  : int  516 55 303 710 1587 3049 1 10 348 3192 ...
#>  $ clearance_rate: int  323 50 173 480 726 1223 1 5 185 1507 ...
```

The `nfs_panel` dataset contains state-level panel data with staggered
adoption of non-fatal strangulation laws.

### Running a basic power analysis

``` r
# Register parallel backend for speed (optional)
# library(doParallel)
# cl <- makeCluster(detectCores() - 1)
# registerDoParallel(cl)

result <- run_power_analysis(
  data_clean = nfs_panel,
  outcome = "clearance_rate",
  unit_var = "state",
  time_var = "year",
  group_var = "treatment_year",
  treat_ind_var = "treated",
  percent_effect = 1.10,
  pta_type = "imputation",
  models_to_run = c("imputation"),
  n_sims = 50,
  noise_spec = list(engine = "iid")
)

# View power results
result$final_power
```

### Interpreting results

The output includes: - `power`: Share of simulations where the effect is
statistically significant (\|t\| \> 1.96) - `mean_att`: Average
estimated treatment effect across simulations - `mean_se`: Average
standard error - `percent_effect`: The multiplicative effect size used
(1.10 = 10% increase)

## Choosing an estimator

staggeredpower supports multiple heterogeneity-robust estimators via
adapters:

| Adapter                                                                                                          | Estimator                   | Required Package        |
|------------------------------------------------------------------------------------------------------------------|-----------------------------|-------------------------|
| [`adapter_imputation()`](https://zjelveh.github.io/staggeredpower/reference/adapter_imputation.md)               | Borusyak et al. (2024)      | `didimputation`         |
| [`adapter_cs()`](https://zjelveh.github.io/staggeredpower/reference/adapter_cs.md)                               | Callaway & Sant’Anna (2021) | `did`                   |
| [`adapter_did2s()`](https://zjelveh.github.io/staggeredpower/reference/adapter_did2s.md)                         | Gardner (2022)              | `did2s`                 |
| [`adapter_etwfe_poisson_glm()`](https://zjelveh.github.io/staggeredpower/reference/adapter_etwfe_poisson_glm.md) | Wooldridge (2023) ETWFE     | (built-in via `fixest`) |

Install only the packages you need:

``` r
install.packages("didimputation")  # for adapter_imputation
install.packages("did")            # for adapter_cs
```

## Next steps

See
[`vignette("advanced-usage")`](https://zjelveh.github.io/staggeredpower/articles/advanced-usage.md)
for grid search, noise engines, and custom adapters.
