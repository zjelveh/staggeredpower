# staggeredpower CRAN Publication Design

**Date:** 2026-03-15
**Status:** Approved
**Approach:** Community-Ready Release (Approach 2)

## Overview

Prepare the `staggeredpower` R package for CRAN publication as a community tool for power analysis in staggered difference-in-differences designs. The package is already functionally mature (16 R source files, 29 functions with `@export` tags in source, 14 test files). Note: NAMESPACE is stale (22 exports) and will be rebuilt via `devtools::document()` after export changes. This workstream addresses infrastructure, API surface, documentation, CI/CD, and submission requirements.

**GitHub:** `github.com/zjelveh/staggeredpower` (public repo, already exists)

---

## Section 1: API Surface Curation

### Exported Functions (14)

| Function | Role |
|----------|------|
| `run_power_analysis()` | Core entry point for single power analysis |
| `run_power_grid()` | Grid search over multiple parameter combinations |
| `estimate_models()` | Run estimators on panel data (used in paper estimation scripts) |
| `enforce_PTA()` | Parallel trends assumption enforcement dispatcher |
| `adapter_cs()` | Callaway-Sant'Anna estimator adapter |
| `adapter_did2s()` | DID2S estimator adapter |
| `adapter_imputation()` | TWFE imputation estimator adapter |
| `adapter_etwfe_poisson_glm()` | ETWFE Poisson GLM estimator adapter |
| `register_adapter()` | Register custom user-defined adapters |
| `get_adapter()` | Retrieve adapter by name |
| `list_adapters()` | List all registered adapters |
| `compute_pretrend_wald_test()` | Pre-trend diagnostic testing |
| `load_config()` | Load YAML configuration files |
| `store_results()` | Aggregate and return model results (refactored to be generic) |

### Demoted to Internal (15)

| Function | Reason |
|----------|--------|
| `enforce_PTA_imputation()` | Implementation detail — users go through `enforce_PTA()` |
| `enforce_PTA_poisson()` | Implementation detail — users go through `enforce_PTA()` |
| `estimator_adapter()` | Base constructor — users use specific `adapter_*()` functions |
| `compute_te()` | Internal computation helper |
| `create_wide()` | Data reshaping utility |
| `standard_estimate()` | Internal data structure constructor |
| `compute_cv_comparison()` | Specialized internal utility |
| `sample_lognormal_with_mean()` | Statistical utility helper |
| `run_vanilla_poisson_es()` | Specialized internal function |
| `normalize_noise_spec()` | Noise engine internal — calibration detail |
| `calibrate_noise_imputation()` | Noise engine internal — calibration detail |
| `calibrate_noise_poisson()` | Noise engine internal — calibration detail |
| `calibrate_noise_cs()` | Noise engine internal — calibration detail |
| `draw_noise()` | Noise engine internal — generation detail |
| `draw_noise_cs()` | Noise engine internal — generation detail |

**Implementation:** Remove `@export` tags from demoted functions. Internal calls remain unchanged (same-package access doesn't require export).

---

## Section 2: Dependency Management

### Hard Dependencies (`Imports`)

| Package | Reason |
|---------|--------|
| `data.table` | Core data structure used throughout |
| `fixest` | Used by imputation adapter, noise calibration, pretrend testing, PTA enforcement |

### Soft Dependencies (`Suggests`)

| Package | Guard Location |
|---------|---------------|
| `did` | `adapter_cs()` |
| `did2s` | `adapter_did2s()` |
| `didimputation` | `adapter_imputation()` |
| `foreach` | Parallel code paths in `run_power_analysis()` / `run_power_grid()` |
| `doParallel` | Parallel code paths |
| `yaml` | `load_config()` |
| `testthat` (>= 3.0.0) | Testing (already present) |
| `knitr` | Vignette building (already present) |
| `rmarkdown` | Vignette building (already present) |
| `ggplot2` | Visualization in vignettes (already present) |

**Removed:** `etwfe` — the ETWFE adapter reimplements the approach via `fixest::fepois()` directly and never calls the `etwfe` package.

### Guard Pattern

Each soft dependency gets a `requireNamespace()` check with an informative error:

```r
if (!requireNamespace("did", quietly = TRUE)) {
  stop("Package 'did' is required for the CS adapter. Install with: install.packages('did')")
}
```

### Parallel Fallback

When `foreach`/`doParallel` aren't installed or `n_cores = 1`, fall back to `base::lapply`. Currently `power_analysis.R` calls `stop()` when `foreach` is missing — this must change to a graceful fallback. Both `run_power_analysis()` and `run_power_grid()` parallel paths need this treatment.

### Net Result

`install.packages("staggeredpower")` pulls in only `data.table` and `fixest`. Users install estimator backends as needed.

---

## Section 3: Infrastructure & CRAN Compliance

### Files to Create

| File | Purpose |
|------|---------|
| `NEWS.md` | Changelog for v0.1.0 initial release |
| `cran-comments.md` | Notes to CRAN reviewers |
| `inst/CITATION` | So `citation("staggeredpower")` returns paper reference |

### Files to Update

| File | Changes |
|------|---------|
| `DESCRIPTION` | `License: GPL (>= 3)`; add `URL` and `BugReports` GitHub fields; restructure `Imports`/`Suggests`; remove `etwfe` from Suggests |
| `.Rbuildignore` | Add `cran-comments.md`, `docs/`, `examples/`, `REFACTORING_ANALYSIS.md`, `^\.github$` |
| `README.md` | Add CRAN badge, `remotes::install_github()` instructions |

### Files to Delete

| File | Reason |
|------|--------|
| `R/.ipynb_checkpoints/` | Contains 5 duplicate `.R` files that will break `R CMD check`. Cannot be excluded via `.Rbuildignore` (operates on top-level paths only). Must be deleted outright. |
| `LICENSE` | GPL-3 doesn't need a separate file (remove reference from DESCRIPTION too) |

### License

GPL-3 (`License: GPL (>= 3)` in DESCRIPTION). Consistent with key dependencies (`data.table`, `fixest`, `did` are all GPL). No separate LICENSE file required — CRAN includes the GPL-3 text automatically.

### .Rbuildignore Additions

```
^cran-comments\.md$
^docs/
^examples/
^REFACTORING_ANALYSIS\.md$
^\.github$
```

### Known Code Issues to Fix

- **Bare `T` instead of `TRUE`:** `store_results.R` line 73 (`na.rm=T`) and `power_analysis.R` line 326 (`na.rm=T`) — must change to `TRUE`
- **`require()` calls in `run_power_grid.R`:** Lines 159 (`require(data.table)`), 301 (`require(foreach)`), 302 (`require(doParallel)`) — CRAN does not permit `require()` or `library()` inside package functions. The `data.table` call is unnecessary (it's in Imports). The `foreach`/`doParallel` calls must become `requireNamespace()` with the graceful `lapply` fallback.
- **`store_results()` hardcoded column names:** References `law_pass` and `year_passed` — must be refactored to accept generic column names for community use
- **`enforce_PTA_imputation()` examples:** Has runnable `@examples` block using `didimputation` — must wrap in `\dontrun{}` or `\donttest{}` before demoting, or remove examples since function becomes internal
- **`\dontrun{}` vs `\donttest{}`:** Audit all examples — CRAN prefers `\donttest{}` for slow-but-valid examples; `\dontrun{}` only for examples that truly cannot run

### R CMD check Target

Zero errors, zero warnings, zero NOTEs. Watch for:
- No `T`/`F` instead of `TRUE`/`FALSE`
- No `:::` calls to other packages
- All `@examples` must run or use `\donttest{}`/`\dontrun{}`
- Package tarball under 5MB
- No files over 5MB

---

## Section 4: Documentation

### Vignette 1: "Getting Started with staggeredpower"

**Audience:** Researcher who just installed the package.

1. What problem does this solve? (2-3 paragraphs)
2. Minimal example: `run_power_analysis()` with a simple staggered design
3. Interpreting the output
4. Choosing an estimator (brief adapter overview)
5. Visualizing results

### Vignette 2: "Advanced Usage"

**Audience:** Researcher who wants full control.

1. Grid search with `run_power_grid()` + YAML config
2. Noise engines (iid, ar1, ar1_anchored) — when to use which
3. PTA enforcement methods
4. Pre-trend testing with `compute_pretrend_wald_test()`
5. Registering custom adapters
6. Result aggregation with `store_results()`
7. Parallel processing setup

### Example Data

Bundle a subset of the paper's panel dataset in `data/` (or `inst/extdata`). Document with roxygen `@docType data`. Monitor size to stay under CRAN's 5MB tarball limit.

### pkgdown Site

- Hosted on GitHub Pages via `gh-pages` branch
- Auto-built by GitHub Actions on push to main
- Reference index grouped by:
  - **Core** — `run_power_analysis()`, `run_power_grid()`, `estimate_models()`
  - **Adapters** — `adapter_cs()`, `adapter_did2s()`, `adapter_imputation()`, `adapter_etwfe_poisson_glm()`
  - **Diagnostics** — `compute_pretrend_wald_test()`, `enforce_PTA()`
  - **Utilities** — `load_config()`, `store_results()`, `register_adapter()`, `get_adapter()`, `list_adapters()`

### Documentation Rebuild

Run `devtools::document()` after all export changes to regenerate `NAMESPACE` and `man/` files. Verify the `man/` directory only contains `.Rd` files for exported functions (internal functions should not have standalone man pages on CRAN, though roxygen2 handles this automatically when `@export` is removed).

---

## Section 5: CI/CD with GitHub Actions

### Workflows

| Workflow | Trigger | Purpose |
|----------|---------|---------|
| `R-CMD-check.yml` | Push to main + PRs | `R CMD check --as-cran` on 4 platform/R combos |
| `pkgdown.yml` | Push to main | Build + deploy pkgdown site to GitHub Pages |
| `test-coverage.yml` | Push to main | `covr::package_coverage()` |

### R-CMD-check Matrix

| OS | R Version |
|----|-----------|
| Ubuntu latest | R-release |
| macOS latest | R-release |
| Windows latest | R-release |
| Ubuntu latest | R-devel |

Uses `r-lib/actions` standard templates.

### Badges

Add to README.md:
- R-CMD-check status badge
- CRAN version badge (post-acceptance)

---

## Section 6: Submission Workflow

### Pre-submission Checklist

1. All prior sections complete
2. `devtools::check()` — 0 errors, 0 warnings, 0 NOTEs locally
3. `rhub::check_for_cran()` — test on CRAN check infrastructure
4. `devtools::spell_check()` — catch typos in documentation
5. `urlchecker::url_check()` — verify all URLs in docs
6. `cran-comments.md` filled out with test environments and results

### Submission

```r
devtools::submit_cran()
```

Wait for CRAN team response (1-5 business days). Address feedback and resubmit if needed.

### Version Strategy

- Submit as `0.1.0` (signals first release, API may evolve)
- Tag GitHub release after CRAN acceptance

---

## Implementation Order

1. **Cleanup** — delete `R/.ipynb_checkpoints/`, fix bare `T`→`TRUE`, remove `LICENSE` file
2. **API curation** — demote 14 functions to internal, verify 14 exports
3. **Dependency restructuring** — Imports → Suggests, add `requireNamespace()` guards, parallel `lapply` fallback
4. **`store_results()` refactoring** — remove hardcoded column names, make generic
5. **Infrastructure files** — GPL-3 license in DESCRIPTION, NEWS.md, cran-comments.md, CITATION, .Rbuildignore
6. **Documentation** — vignettes, pkgdown config, bundled data, `devtools::document()` rebuild
7. **CI/CD** — GitHub Actions workflows
8. **Code linting** — `lintr` pass, `\dontrun` → `\donttest` audit, final `R CMD check`
9. **Pre-submission checks and submit**
