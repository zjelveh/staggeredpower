# staggeredpower CRAN Publication Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Prepare the staggeredpower R package for CRAN publication as a community tool, achieving zero errors/warnings/NOTEs from `R CMD check --as-cran`.

**Architecture:** The package already has a mature adapter-based architecture. This plan curates the public API (14 exported, 15 demoted), moves 6 packages from Imports to Suggests with `requireNamespace()` guards, adds parallel fallback to `lapply`, creates infrastructure files (GPL-3, NEWS.md, vignettes), sets up GitHub Actions CI, and prepares for submission.

**Tech Stack:** R, roxygen2, testthat v3, pkgdown, GitHub Actions (`r-lib/actions`)

**Spec:** `docs/superpowers/specs/2026-03-15-staggeredpower-cran-design.md`

---

## Chunk 1: Cleanup & Code Fixes

### Task 1: Delete `.ipynb_checkpoints` from `R/`

These duplicate `.R` files will cause `R CMD check` failures. They cannot be excluded via `.Rbuildignore`.

**Files:**
- Delete: `R/.ipynb_checkpoints/` (entire directory — 5 files)

- [ ] **Step 1: Delete the directory**

```bash
cd /home/zjelveh/Dropbox/research/base_rate_fallacy/repo/staggeredpower
rm -rf R/.ipynb_checkpoints
```

- [ ] **Step 2: Verify deletion**

```bash
ls R/.ipynb_checkpoints 2>&1
```

Expected: `No such file or directory`

- [ ] **Step 3: Commit**

```bash
git add -A R/.ipynb_checkpoints
git commit -m "chore: remove .ipynb_checkpoints from R/ (breaks R CMD check)"
```

---

### Task 2: Fix bare `T` → `TRUE`

CRAN flags `T`/`F` instead of `TRUE`/`FALSE`. Two known occurrences.

**Files:**
- Modify: `R/store_results.R:73`
- Modify: `R/power_analysis.R:326`

- [ ] **Step 1: Fix `store_results.R` line 73**

Change:
```r
ybar = mean(data[law_pass==0][[outcome]], na.rm=T),
```
To:
```r
ybar = mean(data[law_pass==0][[outcome]], na.rm=TRUE),
```

- [ ] **Step 2: Fix `power_analysis.R` line 326**

Change:
```r
y0_bar = mean(model_data[!is.na(y_cf)][get(treat_ind_var)==0][[outcome]], na.rm=T),
```
To:
```r
y0_bar = mean(model_data[!is.na(y_cf)][get(treat_ind_var)==0][[outcome]], na.rm=TRUE),
```

- [ ] **Step 3: Scan for any other bare `T`/`F` usage**

```bash
cd /home/zjelveh/Dropbox/research/base_rate_fallacy/repo/staggeredpower
grep -rn '=T[,)]' R/ --include="*.R"
grep -rn '=F[,)]' R/ --include="*.R"
```

Expected: No matches (or only within strings/comments).

- [ ] **Step 4: Commit**

```bash
git add R/store_results.R R/power_analysis.R
git commit -m "fix: replace bare T with TRUE for CRAN compliance"
```

---

### Task 3: Remove `require()` calls from `run_power_grid.R`

CRAN does not permit `require()` or `library()` inside package functions. Three occurrences to fix.

**Files:**
- Modify: `R/run_power_grid.R:159` — remove `require(data.table)` (unnecessary, it's in Imports)
- Modify: `R/run_power_grid.R:300-318` — replace `require(foreach)` + `require(doParallel)` with `requireNamespace()` guards

- [ ] **Step 1: Remove the `require(data.table)` call on line 159**

Delete the line:
```r
  require(data.table)
```

(data.table is in Imports — it's always available to package code.)

- [ ] **Step 2: Replace the parallel block (lines 300-318) with guarded version**

Replace the current block:
```r
    if(parallel) {
      require(foreach)
      require(doParallel)

      if(is.null(n_cores)) {
        n_cores <- parallel::detectCores() - 1
      }

      cl <- makeCluster(n_cores)
      registerDoParallel(cl)

      cat(sprintf("Running in parallel with %d cores...\n", n_cores))

      results_list <- foreach(i = 1:nrow(grid),
                             .packages = c("data.table", "staggeredpower")) %dopar% {
        run_single_spec(grid[i])
      }

      stopCluster(cl)
    } else {
```

With:
```r
    if(parallel && requireNamespace("foreach", quietly = TRUE) &&
       requireNamespace("doParallel", quietly = TRUE)) {

      # Local operator assignment — required when foreach is in Suggests
      `%dopar%` <- foreach::`%dopar%`

      if(is.null(n_cores)) {
        n_cores <- parallel::detectCores() - 1
      }

      cl <- parallel::makeCluster(n_cores)
      doParallel::registerDoParallel(cl)
      on.exit(parallel::stopCluster(cl), add = TRUE)

      cat(sprintf("Running in parallel with %d cores...\n", n_cores))

      results_list <- foreach::foreach(i = 1:nrow(grid),
                             .packages = c("data.table", "staggeredpower")) %dopar% {
        run_single_spec(grid[i])
      }

    } else {
      if (parallel) {
        message("Packages 'foreach' and 'doParallel' are needed for parallel execution. ",
                "Install with: install.packages(c('foreach', 'doParallel'))\n",
                "Falling back to sequential execution.")
      }
```

Note: The existing `stopCluster(cl)` on line 318 should be removed since we use `on.exit()` now.

- [ ] **Step 3: Run existing tests to verify nothing breaks**

```bash
cd /home/zjelveh/Dropbox/research/base_rate_fallacy/repo/staggeredpower
Rscript -e "devtools::test()"
```

Expected: All tests pass.

- [ ] **Step 4: Commit**

```bash
git add R/run_power_grid.R
git commit -m "fix: replace require() with requireNamespace() for CRAN compliance"
```

---

## Chunk 2: API Surface Curation

### Task 4: Remove `@export` tags from 15 demoted functions

Remove `@export` from these functions. They remain callable within the package but are no longer part of the public API.

**Files:**
- Modify: `R/enforce_pta.R` — remove `@export` from `enforce_PTA_imputation()` (line 147) and `enforce_PTA_poisson()` (line 540)
- Modify: `R/adapter_base.R` — remove `@export` from `estimator_adapter()` (line 13) and `standard_estimate()` (line 46)
- Modify: `R/compute_te.R` — remove `@export` from `compute_te()` (line 11)
- Modify: `R/data_utils.R` — remove `@export` from `create_wide()` (line 9) and `sample_lognormal_with_mean()` (line 43)
- Modify: `R/pretrend_test.R` — remove `@export` from `run_vanilla_poisson_es()` (line 116) and `compute_cv_comparison()` (line 182)
- Modify: `R/noise_engine.R` — remove `@export` from `normalize_noise_spec()` (line 20), `calibrate_noise_imputation()` (line 85), `calibrate_noise_poisson()` (line 171), `calibrate_noise_cs()` (line 289), `draw_noise()` (line 505), `draw_noise_cs()` (line 672)

- [ ] **Step 1: Remove `@export` from `enforce_pta.R`**

In `R/enforce_pta.R`, remove the `#' @export` lines before `enforce_PTA_imputation` (line 147) and `enforce_PTA_poisson` (line 540). Also remove the runnable `@examples` block from `enforce_PTA_imputation` (lines 137-145) since it becomes internal and uses `didimputation` which will be in Suggests.

Replace the examples block (lines 137-146):
```r
#' @examples
#' df <- data.table(
#'   unit = rep(1:10, each = 5),
#'   time = rep(1:5, 10),
#'   group = rep(c(2,3,4), length.out = 10),
#'   y = rnorm(50),
#'   unemp_rate = runif(50)
#' )
#' result <- enforce_PTA_imputation(df, "unit", "group", "time", "y", "none")
#'
#' @export
```
With:
```r
#' @keywords internal
```

And for `enforce_PTA_poisson`, replace the `@examples` block (lines 527-539) and `#' @export` (line 540) with just `#' @keywords internal`. The examples block to remove is:

```r
#' @examples
#' \dontrun{
#' df <- data.table(
#'   unit = rep(1:10, each = 5),
#'   time = rep(1:5, 10),
#'   group = rep(c(3,4,5), length.out = 10),
#'   count = rpois(50, lambda = 5),
#'   pop = sample(10000:100000, 50, replace = TRUE)
#' )
#' result <- enforce_PTA_poisson(df, "unit", "group", "time", "count",
#'                               pop_var = "pop", outcome_type = "count")
#' }
#'
#' @export
```

Replace all of the above with just `#' @keywords internal`.

- [ ] **Step 2: Remove `@export` from `adapter_base.R`**

In `R/adapter_base.R`:
- Line 13: replace `#' @export` with `#' @keywords internal` (for `estimator_adapter`)
- Line 46: replace `#' @export` with `#' @keywords internal` (for `standard_estimate`)

- [ ] **Step 3: Remove `@export` from `compute_te.R`**

In `R/compute_te.R`, line 11: replace `#' @export` with `#' @keywords internal`.

- [ ] **Step 4: Remove `@export` from `data_utils.R`**

In `R/data_utils.R`:
- Line 9: replace `#' @export` with `#' @keywords internal` (for `create_wide`)
- Line 43: replace `#' @export` with `#' @keywords internal` (for `sample_lognormal_with_mean`)

- [ ] **Step 5: Remove `@export` from `pretrend_test.R`**

In `R/pretrend_test.R`:
- Line 116: replace `#' @export` with `#' @keywords internal` (for `run_vanilla_poisson_es`)
- Line 182: replace `#' @export` with `#' @keywords internal` (for `compute_cv_comparison`)

- [ ] **Step 6: Remove `@export` from `noise_engine.R`**

In `R/noise_engine.R`, replace `#' @export` with `#' @keywords internal` at lines:
- 20 (`normalize_noise_spec`)
- 85 (`calibrate_noise_imputation`)
- 171 (`calibrate_noise_poisson`)
- 289 (`calibrate_noise_cs`)
- 505 (`draw_noise`)
- 672 (`draw_noise_cs`)

- [ ] **Step 7: Rebuild NAMESPACE**

```bash
cd /home/zjelveh/Dropbox/research/base_rate_fallacy/repo/staggeredpower
Rscript -e "devtools::document()"
```

Expected: NAMESPACE should now contain exactly 14 `export()` lines:
`adapter_cs`, `adapter_did2s`, `adapter_etwfe_poisson_glm`, `adapter_imputation`,
`compute_pretrend_wald_test`, `enforce_PTA`, `estimate_models`, `get_adapter`,
`list_adapters`, `load_config`, `register_adapter`, `run_power_analysis`,
`run_power_grid`, `store_results`

- [ ] **Step 8: Verify NAMESPACE**

```bash
grep "^export(" NAMESPACE | wc -l
cat NAMESPACE
```

Expected: 14 export lines.

- [ ] **Step 9: Run tests**

```bash
Rscript -e "devtools::test()"
```

Expected: All tests pass. Internal functions are still accessible within the package.

- [ ] **Step 10: Commit**

```bash
git add R/ NAMESPACE man/
git commit -m "refactor: curate public API surface — 14 exports, 15 demoted to internal"
```

---

## Chunk 3: Dependency Management

### Task 5: Restructure DESCRIPTION Imports/Suggests

Move `did`, `did2s`, `didimputation`, `foreach`, `doParallel`, `yaml` from Imports to Suggests. Remove `etwfe` from Suggests.

**Files:**
- Modify: `DESCRIPTION`

- [ ] **Step 1: Update DESCRIPTION**

Replace the current Imports/Suggests sections:
```
Imports:
    data.table,
    fixest,
    did,
    did2s,
    didimputation,
    foreach,
    doParallel,
    yaml
Suggests:
    testthat (>= 3.0.0),
    knitr,
    rmarkdown,
    ggplot2,
    etwfe
```

With:
```
Imports:
    data.table,
    fixest
Suggests:
    did,
    did2s,
    didimputation,
    foreach,
    doParallel,
    yaml,
    testthat (>= 3.0.0),
    knitr,
    rmarkdown,
    ggplot2
VignetteBuilder: knitr
```

- [ ] **Step 2: Commit**

```bash
git add DESCRIPTION
git commit -m "refactor: move 6 packages from Imports to Suggests for lighter install"
```

---

### Task 6: Add `requireNamespace()` guards to adapter files

Each adapter that calls a Suggests package needs a guard.

**Files:**
- Modify: `R/adapter_cs.R` — guard `did`
- Modify: `R/adapter_did2s.R` — guard `did2s`
- Modify: `R/adapter_imputation.R` — guard `didimputation`
- Modify: `R/config.R` — guard `yaml`

- [ ] **Step 1: Add guard to `adapter_cs.R`**

At the top of the `fit_fn` function body (after line 30 of adapter_cs.R, inside `fit_fn <- function(...) {`), add:

```r
    if (!requireNamespace("did", quietly = TRUE)) {
      stop("Package 'did' is required for the CS adapter. ",
           "Install with: install.packages('did')", call. = FALSE)
    }
```

- [ ] **Step 2: Add guard to `adapter_did2s.R`**

At the top of the `fit_fn` function body, add:

```r
    if (!requireNamespace("did2s", quietly = TRUE)) {
      stop("Package 'did2s' is required for the DID2S adapter. ",
           "Install with: install.packages('did2s')", call. = FALSE)
    }
```

- [ ] **Step 3: Add guard to `adapter_imputation.R`**

At the top of the `fit_fn` function body, add:

```r
    if (!requireNamespace("didimputation", quietly = TRUE)) {
      stop("Package 'didimputation' is required for the imputation adapter. ",
           "Install with: install.packages('didimputation')", call. = FALSE)
    }
```

- [ ] **Step 4: Add guard to `config.R`**

In `R/config.R`, at the top of the `load_config` function body, add:

```r
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required for YAML config loading. ",
         "Install with: install.packages('yaml')", call. = FALSE)
  }
```

- [ ] **Step 5: Commit**

```bash
git add R/adapter_cs.R R/adapter_did2s.R R/adapter_imputation.R R/config.R
git commit -m "feat: add requireNamespace guards for Suggests dependencies"
```

---

### Task 7: Add parallel fallback to `power_analysis.R`

Replace the hard `stop()` when `foreach` is missing with a graceful `lapply` fallback.

**Files:**
- Modify: `R/power_analysis.R:92-119` (parallel check block)
- Modify: `R/power_analysis.R:197-200` (foreach loop)

- [ ] **Step 1: Replace the parallel check block (lines 92-119)**

Replace:
```r
  # Check if parallel backend is registered
  # run_power_analysis uses %dopar% for Monte Carlo simulations
  if (!requireNamespace("foreach", quietly = TRUE)) {
    stop("Package 'foreach' is required but not installed. Please install it with: install.packages('foreach')")
  }

  n_workers <- foreach::getDoParWorkers()

  if (n_workers == 1 && n_sims > 1L) {
    warning(
      "\n================================================================================\n",
      "No parallel backend detected. Monte Carlo simulations will run SEQUENTIALLY.\n",
      "This will be very slow for n_sims > 10.\n\n",
      "For faster execution, register a parallel backend BEFORE calling this function:\n\n",
      "  library(doParallel)\n",
      "  cl <- makeCluster(detectCores() - 1)\n",
      "  registerDoParallel(cl)\n",
      "  # ... run your analysis ...\n",
      "  stopCluster(cl)\n\n",
      "See ?registerDoParallel for details.\n",
      "================================================================================\n",
      call. = FALSE,
      immediate. = TRUE
    )
  } else if (n_workers > 1) {
    cat(sprintf("Using %d cores for Monte Carlo simulations (%d total sims)\n",
                n_workers, n_sims))
  }
```

With:
```r
  # Determine parallel vs sequential execution
  use_parallel <- requireNamespace("foreach", quietly = TRUE)

  if (use_parallel) {
    n_workers <- foreach::getDoParWorkers()
    if (n_workers > 1) {
      cat(sprintf("Using %d cores for Monte Carlo simulations (%d total sims)\n",
                  n_workers, n_sims))
    } else if (n_sims > 1L) {
      message("No parallel backend detected. Running sequentially. ",
              "For faster execution, register a backend:\n",
              "  library(doParallel); cl <- makeCluster(detectCores() - 1); registerDoParallel(cl)")
    }
  } else if (n_sims > 1L) {
    message("Package 'foreach' not installed. Running sequentially. ",
            "For parallel execution: install.packages(c('foreach', 'doParallel'))")
  }
```

- [ ] **Step 2: Restructure the `foreach` loop (lines 197-339) into function + dispatch**

The current `foreach` block spans lines 197-339. The simulation body (lines 201-338) must be extracted into a standalone function, then dispatched via `foreach` or `lapply`.

**Replace lines 197-339** (the entire `rez_list = foreach(...) %dopar% { ... }` block):

```r
  # Extract simulation body into a function
  sim_fn <- function(sim) {
      # CRITICAL: Disable fixest internal threading to prevent race conditions
      # when running multiple simulations per worker. Without this, fepois()
      # causes heap corruption. See: https://github.com/lrberge/fixest/issues/157
      fixest::setFixest_nthreads(1)

      new_temp = list()

      # ... (lines 210-336 remain unchanged — the entire body of the
      #      original foreach block, starting from enforce_PTA() call
      #      through return(returnz)) ...

      return(returnz)
  }

  # Dispatch: parallel if foreach available and backend registered, else sequential
  if (use_parallel && foreach::getDoParWorkers() > 1) {
    # Local operator assignment — required when foreach is in Suggests
    `%dopar%` <- foreach::`%dopar%`

    rez_list <- foreach::foreach(sim = 1:n_sims,
              .packages = c('data.table', 'staggeredpower')) %dopar% {
      sim_fn(sim)
    }
  } else {
    rez_list <- lapply(1:n_sims, sim_fn)
  }
```

The key changes:
1. Lines 210-338 (the `enforce_PTA()` call through `return(returnz)`) become the body of `sim_fn`
2. The `foreach(...)  %dopar%` wrapper is replaced with the dispatch block
3. `%dopar%` is locally assigned from `foreach` namespace (required when in Suggests)
4. `.packages` list simplified — `did`, `did2s`, `didimputation`, `etwfe` removed (loaded by adapters via `requireNamespace()`), `fixest` removed (in Imports)
5. `sim` variable is passed as a function argument instead of captured from `foreach`

- [ ] **Step 3: Run tests**

```bash
cd /home/zjelveh/Dropbox/research/base_rate_fallacy/repo/staggeredpower
Rscript -e "devtools::test()"
```

Expected: All tests pass.

- [ ] **Step 4: Commit**

```bash
git add R/power_analysis.R
git commit -m "feat: graceful lapply fallback when foreach/doParallel not installed"
```

---

### Task 8: Update `.onLoad()` adapter registration

The `zzz.R` auto-registration needs to handle Suggests packages gracefully — it already does for `did`, `did2s`, `didimputation` via `requireNamespace()` checks. But `fixest` is now in Imports, so the ETWFE adapter registration guard is redundant (fixest is always available). Update for clarity.

**Files:**
- Modify: `R/zzz.R`

- [ ] **Step 1: Update `.onLoad()`**

Replace:
```r
  # Saturated Poisson ETWFE (Wooldridge 2023) via fixest
  if (requireNamespace("fixest", quietly = TRUE)) {
    register_adapter(adapter_etwfe_poisson_glm())  # registers as "etwfe_poisson"
  }
```

With:
```r
  # Saturated Poisson ETWFE (Wooldridge 2023) via fixest (always available — in Imports)
  register_adapter(adapter_etwfe_poisson_glm())
```

- [ ] **Step 2: Commit**

```bash
git add R/zzz.R
git commit -m "chore: simplify .onLoad() — fixest is in Imports, always available"
```

---

## Chunk 4: `store_results()` Refactoring

### Task 9: Make `store_results()` generic (remove hardcoded column names)

The current function references `law_pass` and `year_passed` — domain-specific column names from the research paper. For a community package, these must be parameterized.

**Files:**
- Modify: `R/store_results.R`
- Create: `tests/testthat/test-store-results.R`

- [ ] **Step 1: Write the test**

Create `tests/testthat/test-store-results.R`:

```r
test_that("store_results works with generic column names", {
  # Simulate a CS-style result
  mock_results <- list(
    cs = list(
      agg = list(overall.att = 0.05, overall.se = 0.02),
      ev = list(att.egt = c(0.01, 0.03, 0.05), se.egt = c(0.02, 0.02, 0.03), egt = c(-2, -1, 0))
    )
  )

  # Create mock data with generic column names
  mock_data <- data.table::data.table(
    treatment_year = rep(c(2005, 2010, NA), each = 10),
    treated = rep(c(1, 1, 0), each = 10),
    outcome_var = rnorm(30)
  )

  agg <- list()
  ev <- list()

  result <- store_results(
    results = mock_results,
    data = mock_data,
    aggregate_results = agg,
    event_study_results = ev,
    pta_type = "cs",
    enforce_type = NULL,
    analysis_level = "county",
    outcome = "outcome_var",
    use_controls = NULL,
    drop_add_states = "none",
    result_type = "main",
    transform_outcome = NULL,
    treat_col = "treated",
    group_col = "treatment_year"
  )

  expect_length(result$aggregate_results, 1)
  expect_length(result$event_study_results, 1)
  expect_true("att" %in% names(result$aggregate_results[[1]]))
  expect_true("ybar" %in% names(result$aggregate_results[[1]]))
})

test_that("store_results backward compat with default column names", {
  # Ensure the function works without specifying treat_col/group_col
  # when data has the legacy column names
  mock_results <- list(
    imputation = list(
      agg = list(estimate = 0.03, std.error = 0.01),
      ev = list(estimate = c(0.01, 0.03), std.error = c(0.01, 0.02), term = c(-1, 0))
    )
  )

  mock_data <- data.table::data.table(
    year_passed = rep(c(2005, NA), each = 10),
    law_pass = rep(c(1, 0), each = 10),
    outcome_var = rnorm(20)
  )

  result <- store_results(
    results = mock_results,
    data = mock_data,
    aggregate_results = list(),
    event_study_results = list(),
    pta_type = "imputation",
    enforce_type = NULL,
    analysis_level = "county",
    outcome = "outcome_var",
    use_controls = NULL,
    drop_add_states = "none",
    result_type = "main",
    transform_outcome = NULL
  )

  expect_length(result$aggregate_results, 1)
})
```

- [ ] **Step 2: Run the test to verify it fails**

```bash
Rscript -e "devtools::test(filter='store-results')"
```

Expected: FAIL — `unused argument (treat_col = "treated")`

- [ ] **Step 3: Refactor `store_results()`**

Replace the entire `R/store_results.R` with:

```r
# R/store_results.R
#' Store Power Analysis Results
#'
#' Aggregates estimation results from multiple models into structured data.tables
#' for further analysis. Extracts ATT estimates, standard errors, and event study
#' coefficients from CS, imputation, ETWFE, and TWFE model outputs.
#'
#' @param results Named list of model results (from estimate_models())
#' @param data data.table of the analysis dataset
#' @param aggregate_results List to append aggregate results to
#' @param event_study_results List to append event study results to
#' @param pta_type PTA enforcement type used
#' @param enforce_type Enforcement type specification
#' @param analysis_level Analysis level (e.g., "county", "state")
#' @param outcome Outcome variable name
#' @param use_controls Control variables used
#' @param drop_add_states State dropping specification
#' @param result_type Result type label
#' @param transform_outcome Outcome transformation applied
#' @param treat_col Character. Column indicating treatment status (0/1).
#'   Default "law_pass" for backward compatibility.
#' @param group_col Character. Column indicating treatment group/cohort.
#'   Default "year_passed" for backward compatibility.
#'
#' @return List with two elements: aggregate_results and event_study_results,
#'   each a list of data.tables
#' @export
store_results <- function(results,
                          data,
                          aggregate_results,
                          event_study_results,
                          pta_type,
                          enforce_type,
                          analysis_level,
                          outcome,
                          use_controls,
                          drop_add_states,
                          result_type,
                          transform_outcome,
                          treat_col = "law_pass",
                          group_col = "year_passed") {
  for(model in names(results)) {
    if(model %in% c('etwfe', 'imputation')){
      att = results[[model]]$agg$estimate
      se =  results[[model]]$agg$std.error
      att_ev = results[[model]]$ev$estimate
      se_ev = results[[model]]$ev$std.error
      if(model == 'etwfe'){
        rel_pass = results[[model]]$ev$event
      } else{
        rel_pass = results[[model]]$ev$term
      }
    }


    if(model == 'cs'){
      att = results[[model]]$agg$overall.att
      se =  results[[model]]$agg$overall.se
      att_ev = results[[model]]$ev$att.egt
      se_ev = results[[model]]$ev$se.egt
      rel_pass = results[[model]]$ev$egt
    }


    if(model %in% c('sa', 'sa2', 'twfe')){
      att = results[[model]]$agg$coeftable[1,1]
      se =  results[[model]]$agg$coeftable[1,2]
      att_ev = results[[model]]$ev$coeftable[,1]
      se_ev = results[[model]]$ev$coeftable[,2]
      rel_pass = rownames(results[[model]]$ev$coeftable)
    }


    aggregate_results[[length(aggregate_results) + 1]] = data.table(
      pta_type = ifelse(is.null(pta_type), NA, pta_type),
      enforce_type = ifelse(is.null(enforce_type), NA, paste0(enforce_type, collapse='*')),
      model = model,
      level = analysis_level,
      outcome = outcome,
      att = att,
      se = se,
      ng = length(unique(data[[group_col]])),
      n = nrow(data),
      controls = paste0(use_controls, collapse='*'),
      ybar = mean(data[get(treat_col) == 0][[outcome]], na.rm = TRUE),
      drop_add_states = drop_add_states,
      result_type = result_type,
      transform_outcome = transform_outcome
    )

    event_study_results[[length(event_study_results) + 1]] = data.table(
      pta_type = ifelse(is.null(pta_type), NA, pta_type),
      enforce_type = ifelse(is.null(enforce_type), NA, paste0(enforce_type, collapse='*')),
      model = model,
      level = analysis_level,
      outcome = outcome,
      rel_pass = rel_pass,
      att = att_ev,
      se = se_ev,
      controls = paste0(use_controls, collapse='*'),
      drop_add_states = drop_add_states,
      result_type = result_type,
      transform_outcome = transform_outcome
    )

  }
  return(list(aggregate_results=aggregate_results,
              event_study_results=event_study_results))

}
```

Key changes:
- Added `treat_col` and `group_col` parameters with backward-compatible defaults
- Replaced `data$year_passed` with `data[[group_col]]`
- Replaced `data[law_pass==0]` with `data[get(treat_col) == 0]`
- Replaced `na.rm=T` with `na.rm = TRUE`

- [ ] **Step 4: Run the test**

```bash
Rscript -e "devtools::test(filter='store-results')"
```

Expected: PASS

- [ ] **Step 5: Run full test suite**

```bash
Rscript -e "devtools::test()"
```

Expected: All tests pass.

- [ ] **Step 6: Commit**

```bash
git add R/store_results.R tests/testthat/test-store-results.R
git commit -m "refactor: make store_results() generic — parameterize column names"
```

---

## Chunk 5: Infrastructure Files

### Task 10: Update DESCRIPTION for GPL-3 and GitHub

**Files:**
- Modify: `DESCRIPTION`

- [ ] **Step 1: Update license and add GitHub fields**

Change:
```
License: MIT + file LICENSE
```
To:
```
License: GPL (>= 3)
```

Add after `RoxygenNote: 7.3.2`:
```
URL: https://github.com/zjelveh/staggeredpower
BugReports: https://github.com/zjelveh/staggeredpower/issues
```

- [ ] **Step 2: Delete the LICENSE file**

```bash
cd /home/zjelveh/Dropbox/research/base_rate_fallacy/repo/staggeredpower
rm -f LICENSE
```

Note: The LICENSE file may not exist (it was missing during spec review). This step is a safety cleanup.

- [ ] **Step 3: Commit**

```bash
git add DESCRIPTION
git rm -f --ignore-unmatch LICENSE
git commit -m "chore: switch to GPL-3 license, add GitHub URL/BugReports"
```

---

### Task 11: Update `.Rbuildignore`

**Files:**
- Modify: `.Rbuildignore`

- [ ] **Step 1: Replace `.Rbuildignore` contents**

```
^staggeredpower\.Rproj$
^\.Rproj\.user$
^examples$
^docs$
^cran-comments\.md$
^REFACTORING_ANALYSIS\.md$
^\.github$
```

- [ ] **Step 2: Commit**

```bash
git add .Rbuildignore
git commit -m "chore: update .Rbuildignore for CRAN submission"
```

---

### Task 12: Create NEWS.md

**Files:**
- Create: `NEWS.md`

- [ ] **Step 1: Write NEWS.md**

```markdown
# staggeredpower 0.1.0

* Initial CRAN release.
* Core functions: `run_power_analysis()`, `run_power_grid()`, `estimate_models()`.
* Pluggable estimator adapters: Callaway-Sant'Anna (`adapter_cs`), Gardner 2SDID (`adapter_did2s`),
  Borusyak-Jaravel-Spiess imputation (`adapter_imputation`), ETWFE Poisson (`adapter_etwfe_poisson_glm`).
* Parallel trends enforcement via `enforce_PTA()` with imputation, CS, and Poisson methods.
* Configurable noise engines: `none` (deterministic), `iid`, `ar1`, `ar1_common`, `ar1_anchored`.
* Pre-trend diagnostic testing with `compute_pretrend_wald_test()`.
* Custom adapter registration via `register_adapter()`.
* YAML-based configuration with `load_config()`.
* Optional parallel execution via `foreach`/`doParallel` with automatic `lapply` fallback.
```

- [ ] **Step 2: Commit**

```bash
git add NEWS.md
git commit -m "docs: add NEWS.md for initial CRAN release"
```

---

### Task 13: Create `cran-comments.md`

**Files:**
- Create: `cran-comments.md`

- [ ] **Step 1: Write template**

```markdown
## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local: Ubuntu 22.04, R 4.x.x
* GitHub Actions: Ubuntu, macOS, Windows (R-release); Ubuntu (R-devel)
* R-hub: (to be filled after rhub::check_for_cran())

## Downstream dependencies

This is a new package with no downstream dependencies.
```

- [ ] **Step 2: Commit**

```bash
git add cran-comments.md
git commit -m "docs: add cran-comments.md template"
```

---

### Task 14: Create `inst/CITATION`

**Files:**
- Create: `inst/CITATION`

- [ ] **Step 1: Create inst directory and CITATION file**

```bash
mkdir -p /home/zjelveh/Dropbox/research/base_rate_fallacy/repo/staggeredpower/inst
```

Write `inst/CITATION`:

```r
bibentry(
  bibtype  = "Manual",
  title    = "staggeredpower: Power Analysis for Staggered Difference-in-Differences Designs",
  author   = person("Zubin", "Jelveh"),
  year     = "2026",
  note     = "R package version 0.1.0",
  url      = "https://github.com/zjelveh/staggeredpower"
)
```

- [ ] **Step 2: Commit**

```bash
git add inst/CITATION
git commit -m "docs: add CITATION file"
```

---

## Chunk 6: Documentation

### Task 15: Bundle example dataset

Create a small panel dataset from the paper's data for use in vignettes and examples.

**Files:**
- Create: `data-raw/create_example_data.R`
- Create: `data/nfs_panel.rda`
- Create: `R/data.R` (roxygen documentation for the dataset)

- [ ] **Step 1: Create `data-raw/` directory**

```bash
mkdir -p /home/zjelveh/Dropbox/research/base_rate_fallacy/repo/staggeredpower/data-raw
mkdir -p /home/zjelveh/Dropbox/research/base_rate_fallacy/repo/staggeredpower/data
```

- [ ] **Step 2: Write the data preparation script**

Create `data-raw/create_example_data.R`. This script should:
1. Read the paper's SHR panel dataset
2. Select a small subset of columns and states (anonymize if needed)
3. Save as `data/nfs_panel.rda` with `usethis::use_data()`

The exact content depends on the source data — the implementing agent should read the paper's data files to determine the right subset. Key requirements:
- Must be < 1MB compressed
- Must include columns: unit ID, time, group (treatment cohort), outcome, population
- Should include enough units and time periods to demonstrate staggered adoption

- [ ] **Step 3: Write dataset documentation**

Create `R/data.R`:

```r
#' Example Staggered DiD Panel Dataset
#'
#' A panel dataset for demonstrating power analysis in staggered
#' difference-in-differences designs. Contains county-level annual data
#' with staggered treatment adoption.
#'
#' @format A data.table with columns:
#' \describe{
#'   \item{unit_id}{Unit identifier}
#'   \item{year}{Year}
#'   \item{treatment_year}{Year unit adopted treatment (NA = never-treated)}
#'   \item{outcome}{Outcome variable}
#'   \item{population}{Population count}
#' }
#'
#' @source Simulated based on patterns from NFS law adoption data.
"nfs_panel"
```

- [ ] **Step 4: Add `data-raw` to `.Rbuildignore`**

Add line to `.Rbuildignore`:
```
^data-raw$
```

- [ ] **Step 5: Rebuild documentation**

```bash
Rscript -e "devtools::document()"
```

- [ ] **Step 6: Commit**

```bash
git add data/ data-raw/ R/data.R .Rbuildignore
git commit -m "feat: add bundled example dataset for vignettes"
```

---

### Task 16: Write Vignette 1 — Getting Started

**Files:**
- Create: `vignettes/getting-started.Rmd`

- [ ] **Step 1: Create vignettes directory**

```bash
mkdir -p /home/zjelveh/Dropbox/research/base_rate_fallacy/repo/staggeredpower/vignettes
```

- [ ] **Step 2: Write the vignette**

Create `vignettes/getting-started.Rmd` with this structure:

```yaml
---
title: "Getting Started with staggeredpower"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Getting Started with staggeredpower}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---
```

Sections:
1. **Why power analysis for staggered DiD?** — 2-3 paragraphs explaining the problem
2. **Quick example** — Load `nfs_panel`, run `run_power_analysis()` with `engine="none"` (deterministic, fast)
3. **Interpreting results** — Walk through the output columns
4. **Choosing an estimator** — Brief overview of available adapters, how to install optional packages
5. **Visualizing power curves** — Simple ggplot2 example

The implementing agent should write the full vignette content using the bundled dataset and real function calls. Use `eval=FALSE` for chunks that require optional packages or take >5 seconds.

- [ ] **Step 3: Build vignette to verify**

```bash
Rscript -e "devtools::build_vignettes()"
```

Expected: Vignette builds without errors.

- [ ] **Step 4: Commit**

```bash
git add vignettes/
git commit -m "docs: add Getting Started vignette"
```

---

### Task 17: Write Vignette 2 — Advanced Usage

**Files:**
- Create: `vignettes/advanced-usage.Rmd`

- [ ] **Step 1: Write the vignette**

Create `vignettes/advanced-usage.Rmd` with:

```yaml
---
title: "Advanced Usage"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Advanced Usage}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---
```

Sections:
1. **Grid search with `run_power_grid()`** — Multiple effect sizes, outcomes, estimators
2. **YAML configuration** — `load_config()` example with a config file
3. **Noise engines** — Compare `engine="none"` vs `engine="ar1"` vs `engine="ar1_anchored"`, when to use each
4. **PTA enforcement methods** — `enforce_PTA()` with different methods
5. **Pre-trend testing** — `compute_pretrend_wald_test()` walkthrough
6. **Custom adapters** — `register_adapter()` example
7. **Parallel processing** — Setup and performance tips

Use `eval=FALSE` for slow/optional-dependency chunks.

- [ ] **Step 2: Build vignette**

```bash
Rscript -e "devtools::build_vignettes()"
```

- [ ] **Step 3: Commit**

```bash
git add vignettes/
git commit -m "docs: add Advanced Usage vignette"
```

---

### Task 18: Configure pkgdown

**Files:**
- Create: `_pkgdown.yml`

- [ ] **Step 1: Write pkgdown config**

Create `_pkgdown.yml`:

```yaml
url: https://zjelveh.github.io/staggeredpower/

template:
  bootstrap: 5

reference:
  - title: "Core Functions"
    desc: "Main entry points for power analysis"
    contents:
      - run_power_analysis
      - run_power_grid
      - estimate_models

  - title: "Estimator Adapters"
    desc: "Pluggable backends for different DiD estimators"
    contents:
      - adapter_cs
      - adapter_did2s
      - adapter_imputation
      - adapter_etwfe_poisson_glm

  - title: "Diagnostics"
    desc: "Pre-trend testing and PTA enforcement"
    contents:
      - compute_pretrend_wald_test
      - enforce_PTA

  - title: "Utilities"
    desc: "Configuration, storage, and adapter management"
    contents:
      - load_config
      - store_results
      - register_adapter
      - get_adapter
      - list_adapters

  - title: "Data"
    contents:
      - nfs_panel

articles:
  - title: "Getting Started"
    contents:
      - getting-started
  - title: "Advanced"
    contents:
      - advanced-usage
```

- [ ] **Step 2: Add to `.Rbuildignore`**

Add line:
```
^_pkgdown\.yml$
```

- [ ] **Step 3: Commit**

```bash
git add _pkgdown.yml .Rbuildignore
git commit -m "docs: add pkgdown configuration"
```

---

## Chunk 7: CI/CD & README

### Task 19: GitHub Actions — R CMD check

**Files:**
- Create: `.github/workflows/R-CMD-check.yaml`

- [ ] **Step 1: Create workflow directory**

```bash
mkdir -p /home/zjelveh/Dropbox/research/base_rate_fallacy/repo/staggeredpower/.github/workflows
```

- [ ] **Step 2: Write R-CMD-check workflow**

Create `.github/workflows/R-CMD-check.yaml`:

```yaml
on:
  push:
    branches: [main, master]
  pull_request:
    branches: [main, master]

name: R-CMD-check

permissions: read-all

jobs:
  R-CMD-check:
    runs-on: ${{ matrix.config.os }}
    name: ${{ matrix.config.os }} (${{ matrix.config.r }})

    strategy:
      fail-fast: false
      matrix:
        config:
          - {os: ubuntu-latest, r: 'release'}
          - {os: macos-latest, r: 'release'}
          - {os: windows-latest, r: 'release'}
          - {os: ubuntu-latest, r: 'devel'}

    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}
      R_KEEP_PKG_SOURCE: yes

    steps:
      - uses: actions/checkout@v4

      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: ${{ matrix.config.r }}
          use-public-rspm: true

      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          extra-packages: any::rcmdcheck
          needs: check

      - uses: r-lib/actions/check-r-package@v2
        with:
          args: 'c("--no-manual", "--as-cran")'
```

- [ ] **Step 3: Commit**

```bash
git add .github/
git commit -m "ci: add R-CMD-check GitHub Actions workflow"
```

---

### Task 20: GitHub Actions — pkgdown

**Files:**
- Create: `.github/workflows/pkgdown.yaml`

- [ ] **Step 1: Write pkgdown workflow**

Create `.github/workflows/pkgdown.yaml`:

```yaml
on:
  push:
    branches: [main, master]
  release:
    types: [published]

name: pkgdown

permissions:
  contents: write

jobs:
  pkgdown:
    runs-on: ubuntu-latest
    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}
    steps:
      - uses: actions/checkout@v4

      - uses: r-lib/actions/setup-r@v2
        with:
          use-public-rspm: true

      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          extra-packages: any::pkgdown, local::.
          needs: website

      - name: Build site
        run: pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)
        shell: Rscript {0}

      - name: Deploy to GitHub pages
        if: github.event_name != 'pull_request'
        uses: JamesIves/github-pages-deploy-action@v4
        with:
          clean: false
          branch: gh-pages
          folder: docs
```

- [ ] **Step 2: Commit**

```bash
git add .github/workflows/pkgdown.yaml
git commit -m "ci: add pkgdown deployment workflow"
```

---

### Task 21: GitHub Actions — test coverage

**Files:**
- Create: `.github/workflows/test-coverage.yaml`

- [ ] **Step 1: Write coverage workflow**

Create `.github/workflows/test-coverage.yaml`:

```yaml
on:
  push:
    branches: [main, master]

name: test-coverage

permissions: read-all

jobs:
  test-coverage:
    runs-on: ubuntu-latest
    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}

    steps:
      - uses: actions/checkout@v4

      - uses: r-lib/actions/setup-r@v2
        with:
          use-public-rspm: true

      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          extra-packages: any::covr, any::xml2
          needs: coverage

      - name: Test coverage
        run: |
          cov <- covr::package_coverage(
            quiet = FALSE,
            clean = FALSE,
            install_path = file.path(normalizePath(Sys.getenv("RUNNER_TEMP"), winslash = "/"), "package")
          )
          covr::to_cobertura(cov)
        shell: Rscript {0}

      - uses: codecov/codecov-action@v4
        with:
          fail_ci_if_error: false
          files: ./cobertura.xml
          token: ${{ secrets.CODECOV_TOKEN }}
```

- [ ] **Step 2: Commit**

```bash
git add .github/workflows/test-coverage.yaml
git commit -m "ci: add test coverage workflow"
```

---

### Task 22: Update README.md

Now that CI workflows exist, add badges and update installation instructions.

**Files:**
- Modify: `README.md`

- [ ] **Step 1: Update README header and installation**

Add badges after the title, update installation instructions to mention CRAN and parallel being optional:

```markdown
# staggeredpower

<!-- badges: start -->
[![R-CMD-check](https://github.com/zjelveh/staggeredpower/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/zjelveh/staggeredpower/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Simulated power analysis for heterogeneity-robust difference-in-difference designs.

## Installation

```r
# Install from GitHub
# install.packages("remotes")
remotes::install_github("zjelveh/staggeredpower")
```

### Optional Dependencies

staggeredpower uses a pluggable adapter architecture. Install the estimators you need:

```r
# Callaway-Sant'Anna
install.packages("did")

# Gardner Two-Stage DiD
install.packages("did2s")

# Borusyak-Jaravel-Spiess Imputation
install.packages("didimputation")

# Parallel processing (recommended for large simulations)
install.packages(c("foreach", "doParallel"))
```
```

- [ ] **Step 2: Update parallel section**

Change "Required Setup (Do This First!)" to "Optional: Parallel Processing" and soften the language — it's now optional with automatic fallback.

- [ ] **Step 3: Rebuild docs**

```bash
Rscript -e "devtools::document()"
```

- [ ] **Step 4: Commit**

```bash
git add README.md
git commit -m "docs: update README for CRAN — badges, optional deps, softer parallel language"
```

---

## Chunk 8: Final Compliance & Submission Prep

### Task 23: Audit `\dontrun{}` vs `\donttest{}`

CRAN prefers `\donttest{}` for slow-but-valid examples. `\dontrun{}` should only be used for examples that truly cannot run (e.g., require credentials or external services).

**Files:**
- Modify: Any `R/*.R` files with `\dontrun{}` in examples

- [ ] **Step 1: Find all `\dontrun` occurrences**

```bash
cd /home/zjelveh/Dropbox/research/base_rate_fallacy/repo/staggeredpower
grep -rn "dontrun" R/ --include="*.R"
```

- [ ] **Step 2: Replace with `\donttest{}` where appropriate**

For each occurrence, determine if the example:
- Requires optional packages → use `\donttest{}`
- Is slow but valid → use `\donttest{}`
- Truly cannot run → keep `\dontrun{}`

- [ ] **Step 3: Rebuild docs**

```bash
Rscript -e "devtools::document()"
```

- [ ] **Step 4: Commit**

```bash
git add R/ man/
git commit -m "docs: replace dontrun with donttest where examples are valid but slow"
```

---

### Task 24: Full `R CMD check --as-cran`

**Files:** None — verification only.

- [ ] **Step 1: Run check**

```bash
cd /home/zjelveh/Dropbox/research/base_rate_fallacy/repo/staggeredpower
Rscript -e "devtools::check(args = c('--as-cran'))"
```

Expected: 0 errors, 0 warnings, 0 NOTEs.

- [ ] **Step 2: If any issues, fix them**

Common issues at this stage:
- Undocumented arguments in roxygen
- Missing `@return` tags
- Examples that time out (wrap in `\donttest{}`)
- `importFrom` statements that reference packages no longer in Imports

- [ ] **Step 3: Run spell check**

```bash
Rscript -e "devtools::spell_check()"
```

Fix any flagged misspellings in roxygen docs.

- [ ] **Step 4: Check URLs**

```bash
Rscript -e "urlchecker::url_check()"
```

Fix any broken URLs.

- [ ] **Step 5: Commit any fixes**

```bash
git add -A
git commit -m "fix: resolve R CMD check issues for CRAN compliance"
```

---

### Task 25: Update `cran-comments.md` with actual results

- [ ] **Step 1: Fill in the template**

Update `cran-comments.md` with actual R version, platform info, and check results from Task 24.

- [ ] **Step 2: Push everything**

```bash
git push origin main
```

- [ ] **Step 3: Verify CI passes**

Check GitHub Actions at `github.com/zjelveh/staggeredpower/actions` for green builds across all platforms.

---

### Task 26: Submit to CRAN

- [ ] **Step 1: Optional — run `rhub` checks**

```r
rhub::check_for_cran()
```

- [ ] **Step 2: Submit**

```r
devtools::submit_cran()
```

- [ ] **Step 3: Monitor email for CRAN response**

Address any reviewer feedback and resubmit if needed.

- [ ] **Step 4: After acceptance — tag release**

```bash
git tag v0.1.0
git push origin v0.1.0
```

Create a GitHub release at `github.com/zjelveh/staggeredpower/releases`.
