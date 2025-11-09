# Refactoring Analysis: Modularization & Translation Layer Design

## Executive Summary

The current codebase has **model-specific logic scattered across multiple files**. To achieve clean modularity, we need to:

1. **Extract all model-specific I/O into adapter/translator functions**
2. **Define a common internal representation** for counterfactuals, estimates, and aggregations
3. **Make comparison group & time period selection explicit and configurable**
4. **Create a registry pattern** for extensibility

---

## Current Architecture: Where Model Logic Lives

### 1. **Counterfactual Generation** (R/enforce_pta.R)

**Current state**: Two hardcoded methods with embedded logic

```r
# Lines 39-49: Dispatcher
enforce_PTA <- function(df, ..., method = c("imputation", "CS")) {
  method <- match.arg(method)
  if (method == "imputation") {
    enforce_PTA_imputation(...)
  } else {
    enforce_PTA_CS(...)
  }
}
```

**Problems:**
- `method` parameter is hardcoded to two choices
- Comparison group logic embedded in each function:
  - **Imputation** (line 100): `df_untreated <- df[treated == FALSE | is.na(treated)]` (uses all never/not-yet-treated)
  - **CS** (line 193): `control_data = df[get(group) > curr_time | is.na(get(group))]` (not-yet-treated only)
- Time period selection is implicit:
  - **Imputation**: Uses ALL pre-treatment periods (t < g)
  - **CS**: Uses specific pre-period (t = g-1) for each cohort×rel_time cell

**What's good:**
- Already has separate functions for each method
- Returns common format (data.table with 'counterfactual' column)

---

### 2. **Model Estimation** (R/estimate_models.R)

**Current state**: Package-specific code blocks

```r
# Lines 53-100: CS estimation
if('cs' %in% models_to_run){
  # Build formula
  control_formula = as.formula(...)
  # Call did package
  m_csa = att_gt(yname=outcome_var, xformla=control_formula, ...)
  agg_csa <- aggte(m_csa, type='simple', na.rm=T)
  all_results[['cs']][['agg']] = agg_csa
}

# Lines 102-144: TWFE
if('twfe' %in% models_to_run){
  twfe_formula = as.formula(...)
  m1_standard <- feols(twfe_formula, ...)
  all_results[['twfe']][['agg']] = m1_standard
}

# Lines 147-183: Imputation
if('imputation' %in% models_to_run){
  imp_formula = as.formula(...)
  m_imp <- did_imputation(data=..., first_stage=imp_formula, ...)
  all_results[['imputation']][['agg']] = m_imp
}
```

**Problems:**
- Formula construction logic differs for each package
- Control variable handling varies:
  - CS uses `xformla` parameter
  - TWFE includes controls in main formula
  - Imputation uses `first_stage` parameter
- No abstraction layer - raw package calls
- String matching on `models_to_run` vector

**What's good:**
- Already structured as conditional blocks
- Returns named list `all_results[[model_name]]`

---

### 3. **Result Extraction** (R/power_analysis.R)

**Current state**: Hardcoded extraction logic per model

```r
# Lines 212-232: Extract ATT and SE
for(model in names(results)){
  if(model=='imputation'){
    att = results[[model]]$agg$estimate
    se =  results[[model]]$agg$std.error
  }

  if(model=='cs'){
    att = results[[model]]$agg$overall.att
    se =  results[[model]]$agg$overall.se
  }

  if(model%in%c('sa', 'twfe')){
    att = results[[model]]$agg$coeftable[1, 1]
    se =  results[[model]]$agg$coeftable[1, 2]
  }
}
```

**Problems:**
- Each model has different result structure:
  - Imputation: `$agg$estimate`, `$agg$std.error`
  - CS: `$agg$overall.att`, `$agg$overall.se`
  - TWFE: `$agg$coeftable[1, 1]`, `$agg$coeftable[1, 2]`
- Sequential if-statements, no dispatch mechanism
- Model names hardcoded ('imputation', 'cs', 'sa', 'twfe')

**What's good:**
- Already loops over `names(results)` - could work with registry
- Creates common output format (data.table with att, se columns)

---

## Proposed Modular Architecture

### Core Principle: **Translation Layer Pattern**

All model-specific code goes into **adapter functions** that translate between:
- Package-specific inputs → Common internal format
- Common internal format → Package-specific outputs

---

## Refactoring Plan

### Phase 1: Define Common Interfaces

#### 1.1 Counterfactual Specification Interface

```r
# New file: R/counterfactual_spec.R

#' Create counterfactual specification
#' @param comparison_group Function or string defining comparison units
#' @param time_periods Function or string defining time periods to use
#' @param method Name of counterfactual generator
create_cf_spec <- function(comparison_group, time_periods, method) {
  structure(
    list(
      comparison_group = comparison_group,
      time_periods = time_periods,
      method = method
    ),
    class = "cf_spec"
  )
}

# Convenience constructors
cf_spec_cs <- function() {
  create_cf_spec(
    comparison_group = "not_yet_treated",  # g > t or is.na(g)
    time_periods = "last_pre_period",      # t = g - 1
    method = "CS"
  )
}

cf_spec_imputation <- function() {
  create_cf_spec(
    comparison_group = "never_and_not_yet_treated",  # treated == FALSE
    time_periods = "all_pre_periods",                # t < g
    method = "imputation"
  )
}

# Custom spec example
cf_spec_custom <- function(comp_fn, time_fn, method_name) {
  create_cf_spec(
    comparison_group = comp_fn,  # User-supplied function
    time_periods = time_fn,       # User-supplied function
    method = method_name
  )
}
```

#### 1.2 Estimator Adapter Interface

```r
# New file: R/estimator_adapters.R

#' Base class for estimator adapters
#' Each adapter translates between common format and package-specific format
estimator_adapter <- function(name, fit_fn, extract_fn) {
  structure(
    list(
      name = name,
      fit = fit_fn,      # Translates common args → package call
      extract = extract_fn  # Translates package result → common format
    ),
    class = "estimator_adapter"
  )
}

# Common output format
standard_estimate <- function(att, se, model_name, additional_info = list()) {
  list(
    att = att,
    se = se,
    model = model_name,
    info = additional_info
  )
}
```

---

### Phase 2: Implement Adapters

#### 2.1 CS Adapter

```r
# R/adapters/adapter_cs.R

adapter_cs <- function() {

  fit_fn <- function(data, outcome_var, time_var, id_var, group_var,
                     controls, cluster_var, n_cores, ...) {
    # Translate common args to CS-specific format
    if(length(controls) > 0) {
      control_formula <- as.formula(paste0(" ~ ", paste(controls, collapse = " + ")))
    } else {
      control_formula <- as.formula(" ~ 1")
    }

    # Call CS package
    m_csa <- att_gt(
      yname = outcome_var,
      xformla = control_formula,
      tname = time_var,
      idname = id_var,
      gname = group_var,
      data = data,
      allow_unbalanced_panel = FALSE,
      panel = TRUE,
      est_method = 'dr',
      control_group = 'notyettreated',
      clustervars = cluster_var,
      cores = n_cores
    )

    # Aggregate
    agg_csa <- aggte(m_csa, type = 'simple', na.rm = TRUE)

    return(list(raw = m_csa, agg = agg_csa))
  }

  extract_fn <- function(result) {
    # Extract to common format
    standard_estimate(
      att = result$agg$overall.att,
      se = result$agg$overall.se,
      model_name = "cs",
      additional_info = list(
        n_groups = length(unique(result$raw$group)),
        aggregation = "simple"
      )
    )
  }

  estimator_adapter(name = "cs", fit_fn = fit_fn, extract_fn = extract_fn)
}
```

#### 2.2 Imputation Adapter

```r
# R/adapters/adapter_imputation.R

adapter_imputation <- function() {

  fit_fn <- function(data, outcome_var, time_var, id_var, group_var,
                     controls, cluster_var, ...) {
    # Translate common args to imputation-specific format
    if (!is.null(controls) && length(controls) > 0) {
      imp_formula <- as.formula(paste0('~ 0 + ', paste(controls, collapse = " + ")))
    } else {
      imp_formula <- NULL
    }

    # Call didimputation package
    m_imp <- did_imputation(
      data = data[!is.na(get(outcome_var))],
      yname = outcome_var,
      gname = group_var,
      tname = time_var,
      idname = id_var,
      first_stage = imp_formula,
      cluster_var = cluster_var
    )

    return(list(agg = m_imp))
  }

  extract_fn <- function(result) {
    standard_estimate(
      att = result$agg$estimate,
      se = result$agg$std.error,
      model_name = "imputation",
      additional_info = list()
    )
  }

  estimator_adapter(name = "imputation", fit_fn = fit_fn, extract_fn = extract_fn)
}
```

#### 2.3 Sun-Abraham Adapter (Example of extensibility)

```r
# R/adapters/adapter_sunab.R

adapter_sunab <- function() {

  fit_fn <- function(data, outcome_var, time_var, id_var, group_var,
                     controls, cluster_var, treat_ind_var, ...) {
    # Translate to sunab format
    if(length(controls) > 0) {
      control_terms <- paste(controls, collapse = " + ")
      sunab_formula <- as.formula(
        paste0(outcome_var, " ~ sunab(", group_var, ", ", time_var, ") + ",
               control_terms, " | ", id_var, " + ", time_var)
      )
    } else {
      sunab_formula <- as.formula(
        paste0(outcome_var, " ~ sunab(", group_var, ", ", time_var, ") | ",
               id_var, " + ", time_var)
      )
    }

    # Call fixest::sunab via feols
    m_sunab <- feols(sunab_formula, data = data, cluster = cluster_var)

    # Aggregate coefficients
    agg_sunab <- aggregate(m_sunab, agg = "att")

    return(list(raw = m_sunab, agg = agg_sunab))
  }

  extract_fn <- function(result) {
    # Extract ATT estimate
    coef_table <- summary(result$agg)$coefficients

    standard_estimate(
      att = coef_table[1, 1],
      se = coef_table[1, 2],
      model_name = "sunab",
      additional_info = list()
    )
  }

  estimator_adapter(name = "sunab", fit_fn = fit_fn, extract_fn = extract_fn)
}
```

---

### Phase 3: Create Registry

```r
# R/estimator_registry.R

# Global registry
.estimator_registry <- new.env(parent = emptyenv())

#' Register an estimator adapter
register_estimator <- function(adapter) {
  if (!inherits(adapter, "estimator_adapter")) {
    stop("adapter must be an estimator_adapter object")
  }
  .estimator_registry[[adapter$name]] <- adapter
  invisible(adapter)
}

#' Get registered estimator
get_estimator <- function(name) {
  if (!exists(name, envir = .estimator_registry)) {
    stop(sprintf("Estimator '%s' not registered. Available: %s",
                 name, paste(names(.estimator_registry), collapse = ", ")))
  }
  .estimator_registry[[name]]
}

#' List available estimators
list_estimators <- function() {
  names(.estimator_registry)
}

# Register built-in adapters on package load
.onLoad <- function(libname, pkgname) {
  register_estimator(adapter_cs())
  register_estimator(adapter_imputation())
  register_estimator(adapter_sunab())  # If available
}
```

---

### Phase 4: Refactor Core Functions

#### 4.1 New `enforce_PTA()` with explicit specs

```r
# R/enforce_pta.R (refactored)

enforce_PTA <- function(df, unit, group, time, outcome, controls = NULL,
                        cf_spec = NULL, seed = NULL) {

  # Backward compatibility: if cf_spec is NULL, use old 'method' parameter
  if (is.null(cf_spec)) {
    warning("Using deprecated 'method' parameter. Please use cf_spec instead.")
    method <- match.arg(method, c("imputation", "CS"))
    if (method == "imputation") {
      cf_spec <- cf_spec_imputation()
    } else {
      cf_spec <- cf_spec_cs()
    }
  }

  # Dispatch based on cf_spec$method
  if (cf_spec$method == "CS") {
    enforce_PTA_CS(df, unit, group, time, outcome, controls, seed, cf_spec)
  } else if (cf_spec$method == "imputation") {
    enforce_PTA_imputation(df, unit, group, time, outcome, controls, seed, cf_spec)
  } else {
    # Custom counterfactual generator
    enforce_PTA_custom(df, unit, group, time, outcome, controls, seed, cf_spec)
  }
}

# Updated internal functions now accept cf_spec and use it to determine
# comparison groups and time periods
```

#### 4.2 New `estimate_models()` with registry

```r
# R/estimate_models.R (refactored)

estimate_models <- function(data, id_var, outcome_var, time_var, group_var,
                            controls, models_to_run, cluster_var = NULL,
                            treat_ind_var = 'law_pass', n_cores = NULL, ...) {

  # Defaults
  if (is.null(cluster_var)) cluster_var <- id_var
  if (is.null(n_cores)) n_cores <- max(1, parallel::detectCores() - 1)

  all_results <- list()

  # Loop over requested models
  for (model_name in models_to_run) {
    # Get adapter from registry
    adapter <- get_estimator(model_name)

    # Call adapter's fit function with common arguments
    result <- adapter$fit(
      data = data,
      outcome_var = outcome_var,
      time_var = time_var,
      id_var = id_var,
      group_var = group_var,
      controls = controls,
      cluster_var = cluster_var,
      treat_ind_var = treat_ind_var,
      n_cores = n_cores,
      ...  # Pass through any additional args
    )

    all_results[[model_name]] <- result
  }

  return(all_results)
}
```

#### 4.3 New result extraction in `power_analysis.R`

```r
# R/power_analysis.R (refactored section)

# Lines 212-232 become:
for (model_name in names(results)) {
  adapter <- get_estimator(model_name)
  estimate <- adapter$extract(results[[model_name]])

  att <- estimate$att
  se <- estimate$se

  # Rest of code unchanged...
}
```

---

## Migration Path

### Backward Compatibility Strategy

**Option 1: Deprecation period**
```r
# Support old 'method' parameter for 1-2 releases
enforce_PTA <- function(df, ..., method = NULL, cf_spec = NULL) {
  if (!is.null(method) && is.null(cf_spec)) {
    .Deprecated(msg = "Parameter 'method' is deprecated. Use 'cf_spec' instead.")
    # Auto-convert
    cf_spec <- if (method == "CS") cf_spec_cs() else cf_spec_imputation()
  }
  # ... new code
}
```

**Option 2: Parallel implementation**
```r
# Keep old functions as-is, add new ones
enforce_PTA_v2(..., cf_spec)  # New version
estimate_models_v2(..., estimators)  # New version

# Gradually migrate examples/vignettes
```

---

## Benefits of This Approach

### 1. **Clean Separation**
- **Model-agnostic code**: `run_power_analysis()`, `run_power_grid()` never mention specific packages
- **Model-specific code**: Isolated in `R/adapters/` directory
- **Common interface**: All adapters speak same language

### 2. **Extensibility**
```r
# User can add new estimator without touching core code
my_adapter <- estimator_adapter(
  name = "my_method",
  fit_fn = function(...) { ... },
  extract_fn = function(...) { ... }
)
register_estimator(my_adapter)

# Now works everywhere
run_power_analysis(..., models_to_run = c("cs", "my_method"))
```

### 3. **Fine-Grained Control**
```r
# User can customize comparison groups
my_cf_spec <- create_cf_spec(
  comparison_group = function(df, time_var, group_var) {
    # Custom logic: only use units treated 3+ years later
    df[get(group_var) > get(time_var) + 3]
  },
  time_periods = "last_two_pre_periods",
  method = "CS"
)

enforce_PTA(..., cf_spec = my_cf_spec)
```

### 4. **Testing**
- Can test each adapter independently
- Mock adapters for unit tests
- Easier to verify translations are correct

---

## What Needs to Change in Each File

### High-priority changes (core refactoring):

**R/enforce_pta.R** (enforce_PTA, enforce_PTA_CS, enforce_PTA_imputation)
- ✓ Already has separate functions - good structure
- ✗ Comparison group logic hardcoded in each (lines 100, 193)
- ✗ Time period selection implicit
- **Change**: Add `cf_spec` parameter, extract comparison/time logic into helpers

**R/estimate_models.R** (estimate_models)
- ✗ Sequential if-blocks for each model (lines 53-183)
- ✗ Formula construction differs per package
- **Change**: Replace if-blocks with registry lookup + adapter dispatch

**R/power_analysis.R** (run_power_analysis, lines 212-232)
- ✗ Hardcoded result extraction per model
- **Change**: Use `adapter$extract()` instead of if-statements

### Medium-priority (API improvements):

**R/run_power_grid.R** (run_power_grid)
- ✓ Already accepts `models_to_run` as parameter
- ✗ Parameter validation assumes specific models
- **Change**: Use `list_estimators()` for validation

### Low-priority (nice-to-have):

**examples/** scripts
- Update to use new `cf_spec` interface (optional during deprecation)
- Demonstrate custom adapters

---

## Concrete Next Steps

### Phase 0: Preparation (Week 1)

**Goal**: Set up testing infrastructure before making changes

1. **Create test fixtures**
   - Extract small test dataset from strangulation data
   - Save as `tests/testthat/fixtures/test_data.rds`
   - Should have ~5 units, 10 time periods, 2-3 treatment cohorts

2. **Write integration tests for current behavior**
   ```r
   # tests/testthat/test-current-behavior.R
   test_that("CS estimator produces expected output", {
     results <- estimate_models(..., models_to_run = "cs")
     expect_true("cs" %in% names(results))
     expect_true("agg" %in% names(results$cs))
   })
   ```

3. **Document current output formats**
   - What does `results$cs$agg` contain?
   - What does `results$imputation$agg` contain?
   - Create R/adapters/OUTPUT_FORMATS.md

---

### Phase 1: Build Adapter Infrastructure (Week 2)

**Goal**: Create the adapter framework without breaking existing code

#### Step 1.1: Create adapter base classes

```r
# R/adapters/adapter_base.R

#' @title Estimator Adapter Base Class
#' @description Defines the interface all estimator adapters must implement
estimator_adapter <- function(name, fit_fn, extract_fn, requires = NULL) {
  structure(
    list(
      name = name,
      fit = fit_fn,
      extract = extract_fn,
      requires = requires  # Package dependencies
    ),
    class = "estimator_adapter"
  )
}

#' @title Standard estimate format
#' @description Common output all adapters must return
standard_estimate <- function(att, se, model_name, n_units = NA, n_groups = NA,
                             additional_info = list()) {
  structure(
    list(
      att = att,
      se = se,
      model = model_name,
      n_units = n_units,
      n_groups = n_groups,
      info = additional_info
    ),
    class = "standard_estimate"
  )
}

# Print method
print.standard_estimate <- function(x, ...) {
  cat(sprintf("Model: %s\n", x$model))
  cat(sprintf("ATT:   %.4f (SE: %.4f)\n", x$att, x$se))
  if (!is.na(x$n_units)) cat(sprintf("Units: %d\n", x$n_units))
  if (!is.na(x$n_groups)) cat(sprintf("Groups: %d\n", x$n_groups))
}
```

#### Step 1.2: Create registry

```r
# R/adapters/registry.R

.estimator_registry <- new.env(parent = emptyenv())

register_estimator <- function(adapter) {
  stopifnot(inherits(adapter, "estimator_adapter"))
  .estimator_registry[[adapter$name]] <- adapter
  invisible(adapter)
}

get_estimator <- function(name) {
  if (!exists(name, envir = .estimator_registry)) {
    available <- paste(names(.estimator_registry), collapse = ", ")
    stop(sprintf("Estimator '%s' not registered. Available: %s", name, available))
  }
  .estimator_registry[[name]]
}

list_estimators <- function() {
  names(.estimator_registry)
}

has_estimator <- function(name) {
  exists(name, envir = .estimator_registry)
}
```

#### Step 1.3: Write tests for infrastructure

```r
# tests/testthat/test-adapter-infrastructure.R

test_that("can create and register adapter", {
  test_adapter <- estimator_adapter(
    name = "test",
    fit_fn = function(...) list(result = 1),
    extract_fn = function(x) standard_estimate(1, 0.1, "test")
  )

  register_estimator(test_adapter)
  expect_true(has_estimator("test"))
  expect_equal(get_estimator("test")$name, "test")
})

test_that("standard_estimate has required fields", {
  est <- standard_estimate(att = 1.5, se = 0.3, model_name = "test")
  expect_equal(est$att, 1.5)
  expect_equal(est$se, 0.3)
  expect_equal(est$model, "test")
})
```

---

### Phase 2: Write CS Adapter (Week 3)

**Goal**: Create first working adapter by extracting existing CS code

#### Step 2.1: Write CS adapter

```r
# R/adapters/adapter_cs.R

#' @title Callaway-Sant'Anna Adapter
#' @description Adapter for did::att_gt estimator
adapter_cs <- function() {

  fit_fn <- function(data, outcome_var, time_var, id_var, group_var,
                     controls = NULL, cluster_var = NULL, n_cores = NULL,
                     event_study = FALSE, ...) {

    # Package check
    if (!requireNamespace("did", quietly = TRUE)) {
      stop("Package 'did' required for CS estimator. Install with: install.packages('did')")
    }

    # Defaults
    if (is.null(cluster_var)) cluster_var <- id_var
    if (is.null(n_cores)) n_cores <- max(1, parallel::detectCores() - 1)

    # Translate controls to xformla
    if (!is.null(controls) && length(controls) > 0) {
      control_formula <- as.formula(paste0(" ~ ", paste(controls, collapse = " + ")))
    } else {
      control_formula <- as.formula(" ~ 1")
    }

    # Call did package
    m_csa <- did::att_gt(
      yname = outcome_var,
      xformla = control_formula,
      tname = time_var,
      idname = id_var,
      gname = group_var,
      data = data,
      allow_unbalanced_panel = FALSE,
      panel = TRUE,
      est_method = 'dr',
      control_group = 'notyettreated',
      clustervars = cluster_var,
      cores = n_cores
    )

    # Aggregate
    agg_csa <- did::aggte(m_csa, type = 'simple', na.rm = TRUE)

    result <- list(
      raw = m_csa,
      agg = agg_csa
    )

    # Event study if requested
    if (event_study) {
      ev_csa <- did::aggte(m_csa, type = 'dynamic', na.rm = TRUE,
                          min_e = -6, max_e = 10)
      result$ev <- ev_csa
    }

    return(result)
  }

  extract_fn <- function(result) {
    # Extract aggregated results
    att <- result$agg$overall.att
    se <- result$agg$overall.se

    # Get diagnostics
    n_groups <- length(unique(result$raw$group))
    n_units <- length(unique(result$raw$id))

    standard_estimate(
      att = att,
      se = se,
      model_name = "cs",
      n_units = n_units,
      n_groups = n_groups,
      additional_info = list(
        method = "doubly-robust",
        control_group = "not-yet-treated",
        aggregation = "simple"
      )
    )
  }

  estimator_adapter(
    name = "cs",
    fit_fn = fit_fn,
    extract_fn = extract_fn,
    requires = "did"
  )
}
```

#### Step 2.2: Write tests for CS adapter

```r
# tests/testthat/test-adapter-cs.R

test_that("CS adapter produces same results as current code", {
  # Load test data
  test_data <- readRDS("fixtures/test_data.rds")

  # OLD WAY (current code from estimate_models.R)
  old_results <- estimate_models(
    data = test_data,
    id_var = "unit",
    outcome_var = "outcome",
    time_var = "time",
    group_var = "group",
    controls = NULL,
    models_to_run = "cs",
    cluster_var = NULL
  )

  # NEW WAY (using adapter)
  cs_adapter <- adapter_cs()
  new_result_raw <- cs_adapter$fit(
    data = test_data,
    outcome_var = "outcome",
    time_var = "time",
    id_var = "unit",
    group_var = "group",
    controls = NULL,
    cluster_var = NULL
  )
  new_result <- cs_adapter$extract(new_result_raw)

  # Should match
  expect_equal(new_result$att, old_results$cs$agg$overall.att)
  expect_equal(new_result$se, old_results$cs$agg$overall.se)
})

test_that("CS adapter handles controls", {
  test_data <- readRDS("fixtures/test_data.rds")
  test_data$control_var <- rnorm(nrow(test_data))

  cs_adapter <- adapter_cs()
  result_raw <- cs_adapter$fit(
    data = test_data,
    outcome_var = "outcome",
    time_var = "time",
    id_var = "unit",
    group_var = "group",
    controls = "control_var"
  )
  result <- cs_adapter$extract(result_raw)

  expect_true(is.numeric(result$att))
  expect_true(is.numeric(result$se))
})
```

---

### Phase 3: Write Imputation Adapter (Week 3)

**Goal**: Create second adapter to prove pattern works

#### Step 3.1: Write imputation adapter

```r
# R/adapters/adapter_imputation.R

#' @title DID Imputation Adapter
#' @description Adapter for didimputation::did_imputation estimator
adapter_imputation <- function() {

  fit_fn <- function(data, outcome_var, time_var, id_var, group_var,
                     controls = NULL, cluster_var = NULL,
                     event_study = FALSE, ...) {

    # Package check
    if (!requireNamespace("didimputation", quietly = TRUE)) {
      stop("Package 'didimputation' required. Install with: install.packages('didimputation')")
    }

    # Defaults
    if (is.null(cluster_var)) cluster_var <- id_var

    # Translate controls to first_stage formula
    if (!is.null(controls) && length(controls) > 0) {
      imp_formula <- as.formula(paste0('~ 0 + ', paste(controls, collapse = " + ")))
    } else {
      imp_formula <- NULL
    }

    # Call didimputation package
    m_imp <- didimputation::did_imputation(
      data = data[!is.na(get(outcome_var))],
      yname = outcome_var,
      gname = group_var,
      tname = time_var,
      idname = id_var,
      first_stage = imp_formula,
      cluster_var = cluster_var
    )

    result <- list(agg = m_imp)

    # Event study if requested
    if (event_study) {
      ev_imp <- didimputation::did_imputation(
        data = data[!is.na(get(outcome_var))],
        yname = outcome_var,
        gname = group_var,
        tname = time_var,
        idname = id_var,
        first_stage = imp_formula,
        cluster_var = cluster_var,
        horizon = TRUE,
        pretrends = -6:0
      )
      result$ev <- ev_imp
    }

    return(result)
  }

  extract_fn <- function(result) {
    # Extract aggregated results
    att <- result$agg$estimate
    se <- result$agg$std.error

    # Get diagnostics from result
    n_obs <- nrow(result$agg)

    standard_estimate(
      att = att,
      se = se,
      model_name = "imputation",
      additional_info = list(
        method = "imputation",
        n_estimates = n_obs
      )
    )
  }

  estimator_adapter(
    name = "imputation",
    fit_fn = fit_fn,
    extract_fn = extract_fn,
    requires = "didimputation"
  )
}
```

#### Step 3.2: Write tests for imputation adapter

```r
# tests/testthat/test-adapter-imputation.R

test_that("Imputation adapter produces same results as current code", {
  test_data <- readRDS("fixtures/test_data.rds")

  # OLD WAY
  old_results <- estimate_models(
    data = test_data,
    id_var = "unit",
    outcome_var = "outcome",
    time_var = "time",
    group_var = "group",
    controls = NULL,
    models_to_run = "imputation"
  )

  # NEW WAY
  imp_adapter <- adapter_imputation()
  new_result_raw <- imp_adapter$fit(
    data = test_data,
    outcome_var = "outcome",
    time_var = "time",
    id_var = "unit",
    group_var = "group"
  )
  new_result <- imp_adapter$extract(new_result_raw)

  # Should match
  expect_equal(new_result$att, old_results$imputation$agg$estimate)
  expect_equal(new_result$se, old_results$imputation$agg$std.error)
})
```

---

### Phase 4: Parallel Implementation (Week 4)

**Goal**: Make new adapter system work alongside old code

#### Step 4.1: Create estimate_models_v2()

```r
# R/estimate_models_v2.R

#' @title Estimate Models (New Adapter-Based Version)
#' @description Uses adapter registry instead of hardcoded if-blocks
estimate_models_v2 <- function(data, id_var, outcome_var, time_var, group_var,
                               controls = NULL, models_to_run = c("cs", "imputation"),
                               cluster_var = NULL, n_cores = NULL,
                               event_study = FALSE, ...) {

  all_results <- list()

  for (model_name in models_to_run) {
    # Get adapter from registry
    adapter <- get_estimator(model_name)

    # Check package requirements
    if (!is.null(adapter$requires)) {
      if (!requireNamespace(adapter$requires, quietly = TRUE)) {
        warning(sprintf("Skipping %s: package '%s' not installed",
                       model_name, adapter$requires))
        next
      }
    }

    # Call adapter
    result <- adapter$fit(
      data = data,
      outcome_var = outcome_var,
      time_var = time_var,
      id_var = id_var,
      group_var = group_var,
      controls = controls,
      cluster_var = cluster_var,
      n_cores = n_cores,
      event_study = event_study,
      ...
    )

    all_results[[model_name]] <- result
  }

  return(all_results)
}
```

#### Step 4.2: Register adapters on package load

```r
# R/zzz.R

.onLoad <- function(libname, pkgname) {
  # Register built-in adapters
  register_estimator(adapter_cs())
  register_estimator(adapter_imputation())

  # Future: register_estimator(adapter_sunab())
}
```

#### Step 4.3: Write comparison tests

```r
# tests/testthat/test-v1-v2-equivalence.R

test_that("estimate_models and estimate_models_v2 produce same results", {
  test_data <- readRDS("fixtures/test_data.rds")

  v1_results <- estimate_models(
    data = test_data,
    id_var = "unit",
    outcome_var = "outcome",
    time_var = "time",
    group_var = "group",
    models_to_run = c("cs", "imputation")
  )

  v2_results <- estimate_models_v2(
    data = test_data,
    id_var = "unit",
    outcome_var = "outcome",
    time_var = "time",
    group_var = "group",
    models_to_run = c("cs", "imputation")
  )

  # CS should match
  expect_equal(v1_results$cs$agg$overall.att, v2_results$cs$agg$overall.att)

  # Imputation should match
  expect_equal(v1_results$imputation$agg$estimate, v2_results$imputation$agg$estimate)
})
```

---

### Phase 5: Migration (Week 5-6)

**Goal**: Switch codebase to use new adapters

#### Step 5.1: Update power_analysis.R to use adapters

```r
# In R/power_analysis.R, replace lines 212-232:

# OLD CODE:
# for(model in names(results)){
#   if(model=='imputation'){
#     att = results[[model]]$agg$estimate
#     se =  results[[model]]$agg$std.error
#   }
#   if(model=='cs'){
#     att = results[[model]]$agg$overall.att
#     se =  results[[model]]$agg$overall.se
#   }
#   ...
# }

# NEW CODE:
for (model_name in names(results)) {
  adapter <- get_estimator(model_name)
  estimate <- adapter$extract(results[[model_name]])

  att <- estimate$att
  se <- estimate$se

  # Rest of code unchanged...
}
```

#### Step 5.2: Deprecate old estimate_models()

```r
# In R/estimate_models.R

estimate_models <- function(...) {
  .Deprecated("estimate_models_v2",
             msg = "estimate_models() is deprecated. Use estimate_models_v2() instead.")
  estimate_models_v2(...)
}
```

#### Step 5.3: Update all examples

```r
# examples/test_strangulation.R - no changes needed!
# examples/large_scale_analysis.R - no changes needed!
# They already use estimate_models() which now forwards to v2
```

---

### Phase 6: Cleanup (Week 7)

1. **Remove old estimate_models()** entirely
2. **Rename estimate_models_v2()** back to `estimate_models()`
3. **Update documentation**
4. **Write vignette**: "Creating Custom Estimator Adapters"
5. **Add to NAMESPACE**: Export adapter classes and registry functions

---

## Testing Strategy

### Unit Tests (for each adapter)
```r
test_that("adapter interface is complete", {
  adapter <- adapter_cs()
  expect_true(inherits(adapter, "estimator_adapter"))
  expect_true(is.function(adapter$fit))
  expect_true(is.function(adapter$extract))
})

test_that("adapter produces standard_estimate", {
  result <- adapter$extract(fit_result)
  expect_true(inherits(result, "standard_estimate"))
  expect_true(all(c("att", "se", "model") %in% names(result)))
})
```

### Integration Tests
```r
test_that("full pipeline works with adapters", {
  results <- run_power_analysis(
    data_clean = test_data,
    models_to_run = c("cs", "imputation"),
    ...
  )
  expect_true(!is.null(results$final_power))
})
```

### Regression Tests
```r
test_that("adapter results match pre-refactor results", {
  # Load saved results from before refactoring
  old_results <- readRDS("tests/fixtures/pre_refactor_results.rds")

  # Run with new code
  new_results <- run_power_analysis(...)

  # Should be identical
  expect_equal(new_results$final_power$att, old_results$final_power$att)
})
```

---

## File Structure After Refactoring

```
R/
├── adapters/
│   ├── adapter_base.R       # Base classes
│   ├── registry.R            # Registration system
│   ├── adapter_cs.R          # CS adapter
│   ├── adapter_imputation.R  # Imputation adapter
│   ├── adapter_sunab.R       # Sun-Abraham (future)
│   └── OUTPUT_FORMATS.md     # Documentation
├── estimate_models.R         # Now just calls adapters
├── power_analysis.R          # Uses adapter$extract()
└── zzz.R                     # Registers adapters on load

tests/testthat/
├── fixtures/
│   ├── test_data.rds
│   └── pre_refactor_results.rds
├── test-adapter-infrastructure.R
├── test-adapter-cs.R
├── test-adapter-imputation.R
└── test-v1-v2-equivalence.R

vignettes/
└── custom-estimators.Rmd     # How to write your own adapter
```

---

This creates a clean foundation where CS and imputation are just two implementations of a common interface, and adding Sun-Abraham (or any other method) is just a matter of writing a new adapter and registering it!
