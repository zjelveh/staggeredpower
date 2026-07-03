#' Enforce Parallel Trends Assumption
#'
#' @description
#' This function enforces the parallel trends assumption (PTA) by adjusting post-treatment
#' outcomes based on pre-treatment trends. It supports multiple methods for enforcing the PTA
#' and different control group definitions.
#'
#' @param df A data.frame containing panel data
#' @param unit Character. Name of unit identifier column (e.g., 'county_name')
#' @param group Character. Name of treatment group column (e.g., 'year_passed')
#' @param time Character. Name of time column (e.g., 'year')
#' @param outcome Character. Name of outcome column
#' @param controls Character vector. Names of control variables (default: NULL)
#' @param method Character. PTA enforcement method: 'imputation', 'CS', or 'poisson' (default: 'imputation')
#' @param seed Numeric. Random seed for reproducibility (default: NULL)
#' @param pop_var Character. For Poisson method, name of population variable (required for method='poisson')
#' @param outcome_type Character. For Poisson method, 'count' or 'rate'. Default 'rate'.
#' @param trend_type Character. Type of time trend: 'common' (default) uses standard TWFE with
#'   common time effects for all cohorts; 'cohort_trend' estimates cohort-specific polynomial
#'   time trends that can extrapolate to post-treatment periods.
#' @param trend_order Integer. For trend_type='cohort_trend', the polynomial order (1=linear,
#'   2=quadratic, etc.). Default 1. Ignored when trend_type='common'.
#' @param noise_spec List. Noise engine configuration. NULL uses defaults (iid Normal, same
#'   as legacy behavior). See \code{\link{normalize_noise_spec}} for options:
#'   \describe{
#'     \item{engine}{"none" (deterministic), "iid" (default, legacy), "ar1", or "ar1_common"}
#'     \item{innovation}{"normal" (default) or "empirical" (resample from residuals)}
#'     \item{common_shock}{TRUE/FALSE. Whether to include common calendar-year shocks.}
#'     \item{rho}{NULL (auto-estimate) or numeric in \eqn{[0, 0.99]}. AR(1) persistence parameter.}
#'     \item{cs_pool}{"global" (default) or "cohort". How to pool CS innovations.}
#'     \item{obs_model}{"deterministic" or "poisson" (default). For Poisson PTA only.}
#'   }
#' @param control_group Character. For method='CS' only: control group passed to the
#'   CS estimator when harmonizing the DGP with it ("notyettreated" (default) or
#'   "nevertreated"). Ignored by imputation/poisson methods.
#' @param est_method Character. For method='CS' only: did estimation method
#'   ("dr" (default), "reg", or "ipw"). Ignored by other methods.
#' @param base_period Character. For method='CS' only: did base period
#'   ("universal" (default) or "varying"). Ignored by other methods.
#' @param allow_unbalanced_panel Logical. For method='CS' only: passed to
#'   \code{did::att_gt} when deriving the harmonized control counterfactual
#'   (default FALSE). Ignored by other methods.
#'
#' @return A data.frame with enforced parallel trends (includes 'counterfactual' column)
#'
#' @details
#' Three methods are available:
#' \describe{
#'   \item{imputation}{Uses two-way fixed effects on untreated observations to impute
#'     counterfactuals. Assumes additive parallel trends on level scale.}
#'   \item{CS}{Uses Callaway-Sant'Anna style approach with not-yet-treated controls.
#'     Assumes additive parallel trends on level scale.}
#'   \item{poisson}{Uses Poisson regression with unit and time fixed effects on untreated
#'     observations. Assumes multiplicative parallel trends on log-rate scale. This is
#'     appropriate for rare count outcomes (Wooldridge 2023).}
#' }
#'
#' @examples
#' \donttest{
#' # Linear approach (additive PT) using bundled dataset
#' pta_results <- enforce_PTA(
#'   df = nfs_panel,
#'   unit = "state",
#'   group = "treatment_year",
#'   time = "year",
#'   outcome = "assault_rate",
#'   method = "imputation"
#' )
#' }
#'
#' @importFrom data.table data.table setnames :=
#' @export
enforce_PTA <- function(df, unit, group, time, outcome,
                        controls = NULL,
                        method = c("imputation", "CS", "poisson"),
                        seed = NULL,
                        pop_var = NULL,
                        outcome_type = "rate",
                        trend_type = c("common", "cohort_trend"),
                        trend_order = 1L,
                        noise_spec = NULL,
                        control_group = "notyettreated",
                        est_method = "dr",
                        base_period = "universal",
                        allow_unbalanced_panel = FALSE) {
  method <- match.arg(method)
  trend_type <- match.arg(trend_type)
  trend_order <- as.integer(trend_order)
  noise_spec <- normalize_noise_spec(noise_spec)

  # Validate trend_order

  if (trend_type == "cohort_trend" && trend_order < 1L) {
    stop("trend_order must be >= 1 for cohort_trend")
  }

  if (method == "imputation") {
    enforce_PTA_imputation(df, unit, group, time, outcome, controls, seed,
                           trend_type, trend_order, noise_spec)
  } else if (method == "CS") {
    # CS already has cohort-specific trends implicitly via not-yet-treated controls
    # trend_type and trend_order are ignored for CS method. The did-estimator
    # settings (control_group/est_method/base_period/allow_unbalanced_panel) are
    # forwarded so the DGP is HARMONIZED with the CS estimator applied downstream.
    enforce_PTA_CS(df, unit, group, time, outcome, controls, seed, noise_spec,
                   control_group = control_group, est_method = est_method,
                   base_period = base_period,
                   allow_unbalanced_panel = allow_unbalanced_panel)
  } else if (method == "poisson") {
    enforce_PTA_poisson(df, unit, group, time, outcome, controls, seed,
                        pop_var, outcome_type, trend_type, trend_order, noise_spec)
  }
}


#' Generate counterfactual outcomes using imputation approach
#' 
#' @description
#' This function implements the imputation approach for difference-in-differences with staggered adoption.
#' It estimates fixed effects using only untreated observations and generates counterfactual outcomes
#' for treated observations.
#' 
#' @param df A data.table containing the panel data
#' @param unit Character string specifying the column name for unit identifiers
#' @param group Character string specifying the column name for treatment group/cohort
#' @param time Character string specifying the column name for time periods
#' @param outcome Character string specifying the column name for the outcome variable
#' @param controls Character string specifying whether to include controls ("controls") or 
#'   use only fixed effects ("none")
#'
#' @return A data.table with an additional column 'counterfactual' containing:
#'   - Actual outcomes for untreated observations
#'   - Imputed counterfactual outcomes for treated observations
#'
#' @details
#' The function follows these steps:
#' 1. Splits data into treated and untreated observations
#' 2. Estimates fixed effects model on untreated observations only
#' 3. Uses these estimates to impute counterfactuals for treated observations
#' 
#' The imputation draws from a normal distribution centered at the predicted mean
#' with variance equal to the residual variance from the first-stage regression.
#'
#' @keywords internal
enforce_PTA_imputation <- function(df, unit, group, time, outcome, controls = NULL, seed = NULL,
                                   trend_type = "common", trend_order = 1L,
                                   noise_spec = NULL) {
  noise_spec <- normalize_noise_spec(noise_spec)
  if (!is.null(seed)) set.seed(seed)

  df <- data.table::as.data.table(df)

  # Create local copies of variable names for data.table scoping
  # This avoids conflicts when parameter names match column names
  unit_col <- unit
  group_col <- group
  time_col <- time
  outcome_col <- outcome

  # Robust treated flag: TRUE if treated unit and at or after treatment time
  # Uses >= to match didimputation's definition (treatment starts at year_passed)
  df[, treated := !is.na(get(group_col)) & get(time_col) >= get(group_col)]

  # Keep untreated observations for FE estimation
  df_untreated <- df[treated == FALSE | is.na(treated)]

  # Create time-centered variable and cohort-specific trend variables for cohort_trend
  # We use separate numeric trend variables (one per cohort × polynomial degree) to avoid NAs
  cohort_trend_vars <- character(0)  # Track created trend variable names
  if (trend_type == "cohort_trend") {
    min_time <- min(df[[time_col]], na.rm = TRUE)
    df[, .time_centered := get(time_col) - min_time]
    df_untreated[, .time_centered := get(time_col) - min_time]

    # Get unique treatment cohorts (excluding never-treated which have NA group)
    treated_cohorts <- sort(unique(df[!is.na(get(group_col))][[group_col]]))

    # Create trend variables for each cohort × polynomial degree
    # Never-treated units get 0 for all trend variables (their trend is captured by common time FE)
    for (cohort in treated_cohorts) {
      for (deg in seq_len(trend_order)) {
        var_name <- paste0(".trend_c", cohort, "_d", deg)
        cohort_trend_vars <- c(cohort_trend_vars, var_name)

        # Value = time_centered^deg if unit belongs to this cohort, 0 otherwise
        df[, (var_name) := fifelse(get(group_col) == cohort, .time_centered^deg, 0, na = 0)]
        df_untreated[, (var_name) := fifelse(get(group_col) == cohort, .time_centered^deg, 0, na = 0)]
      }
    }
  }

  # Build FE formula based on trend_type
  if (trend_type == "common") {
    # Standard TWFE: outcome ~ [controls] | unit + time
    if (!is.null(controls) && length(controls) > 0) {
      control_terms <- paste(controls, collapse = " + ")
      fe_formula <- as.formula(paste0(outcome_col, " ~ ", control_terms, " | ", unit_col, " + ", time_col))
    } else {
      fe_formula <- as.formula(paste0(outcome_col, " ~ 1 | ", unit_col, " + ", time_col))
    }
  } else if (trend_type == "cohort_trend") {
    # Cohort-specific polynomial trends using pre-created trend variables
    # Each variable is cohort-specific: .trend_c{cohort}_d{degree}
    trend_term <- paste(cohort_trend_vars, collapse = " + ")
    if (!is.null(controls) && length(controls) > 0) {
      control_terms <- paste(controls, collapse = " + ")
      fe_formula <- as.formula(paste0(outcome_col, " ~ ", trend_term, " + ", control_terms,
                                      " | ", unit_col, " + ", time_col))
    } else {
      fe_formula <- as.formula(paste0(outcome_col, " ~ ", trend_term, " | ", unit_col, " + ", time_col))
    }
  }

  mod_fe <- fixest::feols(fe_formula, data = df_untreated)
  resid_sd <- stats::sigma(mod_fe)

  # Calibrate noise engine
  noise_calib <- calibrate_noise_imputation(mod_fe, df_untreated, unit_col, time_col, noise_spec)

  # Extract fixed effects
  # Try to preserve the original data type
  unit_names <- names(fixest::fixef(mod_fe)[[unit_col]])
  if(is.numeric(df[[unit_col]])) {
    unit_names <- as.numeric(unit_names)
  }
  unit_effects <- data.table(
    unit = unit_names,
    unit_effect = as.numeric(fixest::fixef(mod_fe)[[unit_col]])
  )
  setnames(unit_effects, "unit", unit_col)  # Rename to match df column name

  time_names <- names(fixest::fixef(mod_fe)[[time_col]])
  if(is.numeric(df[[time_col]])) {
    time_names <- as.numeric(time_names)
  }
  time_effects <- data.table(
    time = time_names,
    time_effect = as.numeric(fixest::fixef(mod_fe)[[time_col]])
  )
  setnames(time_effects, "time", time_col)  # Rename to match df column name

  # Merge effects
  df[unit_effects, on = unit_col, unit_effect := i.unit_effect]
  df[time_effects, on = time_col, time_effect := i.time_effect]

  # Initialize counterfactual as observed
  df[, counterfactual := get(outcome_col)]

  # Predict deterministic counterfactual mu_hat for treated, then add noise
  if (trend_type == "common") {
    # Standard approach: mu_hat = alpha_i + gamma_t + X*beta
    if (!is.null(controls) && length(controls) > 0) {
      control_effects <- coef(mod_fe)[controls]
      control_effects[is.na(control_effects)] <- 0

      df[treated == TRUE, control_effect :=
           Reduce(`+`, Map(function(x, y) get(x) * y, controls, control_effects))]

      df[treated == TRUE, .mu_hat := unit_effect + time_effect + control_effect]
      df[, control_effect := NULL]
    } else {
      df[treated == TRUE, .mu_hat := unit_effect + time_effect]
    }
  } else if (trend_type == "cohort_trend") {
    # Extract all coefficients from model
    all_coefs <- coef(mod_fe)

    # Calculate trend effect for each treated observation
    df[treated == TRUE, .trend_effect := {
      effect <- numeric(.N)
      for (var_name in cohort_trend_vars) {
        if (var_name %in% names(all_coefs) && !is.na(all_coefs[var_name])) {
          effect <- effect + get(var_name) * all_coefs[var_name]
        }
      }
      effect
    }]

    # Compute mu_hat with or without controls
    if (!is.null(controls) && length(controls) > 0) {
      control_coefs <- all_coefs[controls]
      control_coefs[is.na(control_coefs)] <- 0

      df[treated == TRUE, .control_effect :=
           Reduce(`+`, Map(function(x, y) get(x) * y, controls, control_coefs))]

      df[treated == TRUE, .mu_hat := unit_effect + time_effect + .trend_effect + .control_effect]
      df[, .control_effect := NULL]
    } else {
      df[treated == TRUE, .mu_hat := unit_effect + time_effect + .trend_effect]
    }

    # Cleanup cohort_trend-specific columns
    temp_cols <- c(".time_centered", ".trend_effect", cohort_trend_vars)
    existing_cols <- intersect(temp_cols, names(df))
    if (length(existing_cols) > 0) {
      df[, (existing_cols) := NULL]
    }
  }

  # Draw noise and apply to mu_hat
  treated_rows <- df[treated == TRUE]
  if (nrow(treated_rows) > 0) {
    if (noise_spec$obs_model %in% c("poisson", "binomial")) {
      # Count/binomial observation model: return deterministic mean for draw in caller
      df[treated == TRUE, cf_mean_rate := .mu_hat]
      df[treated == TRUE, counterfactual := .mu_hat]
      df[, .mu_hat := NULL]
    } else {
      # Gaussian path: draw noise and apply additively
      eps_dt <- draw_noise(noise_calib,
                            units = treated_rows[[unit_col]],
                            times = treated_rows[[time_col]])
      # Merge eps into df (keyed by unit + time)
      eps_dt_named <- data.table::copy(eps_dt)
      setnames(eps_dt_named, c("unit", "time"), c(unit_col, time_col))
      df[eps_dt_named, on = c(unit_col, time_col), .noise_eps := i.eps]
      df[is.na(.noise_eps), .noise_eps := 0]
      df[treated == TRUE, counterfactual := .mu_hat + .noise_eps]
      df[, c(".mu_hat", ".noise_eps") := NULL]
    }
  }

  # Cleanup common columns
  df[, c("unit_effect", "time_effect", "treated") := NULL]

  return(df)
}




# Session-level memo cache for the CS control counterfactual. The did::att_gt fit is
# invariant across simulations, effect sizes and noise configs (it depends only on the
# real data + estimator settings), so without memoization the power grid would repeat
# the identical fit thousands of times. Lives in the package namespace: persists for the
# session, and each parallel worker gets its own copy (no cross-process sharing needed).
.sp_cs_delta_cache <- new.env(parent = emptyenv())

# Clear the CS delta_control memo cache (mainly for tests / long-running sessions)
.sp_clear_cs_cache <- function() {
  rm(list = ls(.sp_cs_delta_cache, all.names = TRUE), envir = .sp_cs_delta_cache)
  invisible(NULL)
}

#' Back out did's control counterfactual change per (g, t)
#'
#' @description
#' Harmonizes the CS power DGP with the CS estimator. Rather than re-deriving the
#' Callaway--Sant'Anna control counterfactual with independent hand-rolled code
#' (which only matches \code{did::att_gt} on complete-grid / balanced panels), this
#' reads did's OWN control-side straight off a single \code{att_gt} fit on the real
#' data. For each post cell (g, t):
#' \deqn{\Delta_{control}(g,t) = [\text{treated change}] - ATT(g,t)}
#' Because \eqn{ATT(g,t) \equiv [\text{treated change}] - \Delta_{control}(g,t)} by
#' did's definition, the realized treated outcome cancels and \eqn{\Delta_{control}}
#' is a function of the CONTROL outcomes only. Building the null counterfactual as
#' \eqn{Y_{i,g-1} + \Delta_{control}(g,t)} then makes the CS estimator recover ~0 on
#' the null. Exactness requires the DGP and the estimator to use the SAME
#' \code{allow_unbalanced_panel} setting: recovery is machine-zero on balanced panels,
#' and near-zero on ragged panels run with \code{allow_unbalanced_panel = TRUE} (a
#' small residual remains from cells did handles unit-by-unit). With the default
#' \code{allow_unbalanced_panel = FALSE}, did coerces a ragged panel to a balanced
#' subset, so the back-out (which uses the full present-at-both treated set) will not
#' cancel exactly -- pass \code{TRUE} for ragged panels, as the estimator should.
#'
#' @return data.table with columns g, t, delta_control (keyed on g, t).
#' @noRd
.cs_delta_control <- function(df, unit_col, group_col, time_col, outcome_col,
                              controls, control_group, est_method, base_period,
                              allow_unbalanced_panel) {
  if (!requireNamespace("did", quietly = TRUE)) {
    stop("Package 'did' is required for the CS power DGP (pta_type = 'cs'). ",
         "Install with: install.packages('did')", call. = FALSE)
  }

  # Memoize on the VALUES that determine the fit (plain vectors, not the data.table
  # object, whose internal self-reference would otherwise defeat the cache). Returns a
  # fresh copy so callers may re-key/mutate without corrupting the cached result.
  key <- digest::digest(list(
    unit = df[[unit_col]], grp = df[[group_col]], tim = df[[time_col]], out = df[[outcome_col]],
    ctrl = if (!is.null(controls) && length(controls) > 0) {
      lapply(controls, function(cc) df[[cc]])
    } else NULL,
    meta = list(unit_col, group_col, time_col, outcome_col, controls,
                control_group, est_method, base_period, allow_unbalanced_panel)
  ))
  cached <- .sp_cs_delta_cache[[key]]
  if (!is.null(cached)) return(data.table::copy(cached))

  d <- data.table::as.data.table(df)
  # did requires never-treated units coded as 0 (not NA)
  if (any(is.na(d[[group_col]]))) d[is.na(get(group_col)), (group_col) := 0]

  xformla <- if (!is.null(controls) && length(controls) > 0) {
    stats::as.formula(paste0("~ ", paste(controls, collapse = " + ")))
  } else {
    stats::as.formula("~ 1")
  }

  m <- did::att_gt(
    yname = outcome_col, tname = time_col, idname = unit_col, gname = group_col,
    data = as.data.frame(d), xformla = xformla, control_group = control_group,
    anticipation = 0, est_method = est_method, base_period = base_period,
    allow_unbalanced_panel = allow_unbalanced_panel, clustervars = unit_col,
    print_details = FALSE
  )

  gt <- data.table::data.table(g = m$group, t = m$t, att = m$att)
  gt <- gt[g > 0 & t >= g]                 # post-treatment cells only
  gt <- unique(gt, by = c("g", "t"))       # one ATT per (g, t)

  data.table::setkeyv(d, c(unit_col, time_col))
  deltas <- numeric(nrow(gt))
  for (i in seq_len(nrow(gt))) {
    gv <- gt$g[i]; tv <- gt$t[i]; base <- gv - 1
    # base-R filter: avoid NSE collision if group_col is literally named "g"
    tr <- unique(d[[unit_col]][d[[group_col]] == gv & !is.na(d[[group_col]])])
    yb <- d[list(tr, base)][[outcome_col]]
    yt <- d[list(tr, tv)][[outcome_col]]
    ok <- !is.na(yb) & !is.na(yt)
    # treated_change - ATT(g,t): the treated change cancels the treated part inside
    # ATT, leaving did's control counterfactual change (control outcomes only).
    deltas[i] <- if (any(ok)) mean(yt[ok] - yb[ok]) - gt$att[i] else NA_real_
  }
  gt[, delta_control := deltas]
  data.table::setkeyv(gt, c("g", "t"))
  result <- gt[, .(g, t, delta_control)]
  .sp_cs_delta_cache[[key]] <- data.table::copy(result)   # store pristine copy
  result
}


enforce_PTA_CS <- function(df, unit, group, time, outcome, controls = NULL, seed = NULL,
                            noise_spec = NULL,
                            control_group = "notyettreated",
                            est_method = "dr",
                            base_period = "universal",
                            allow_unbalanced_panel = FALSE) {
  noise_spec <- normalize_noise_spec(noise_spec)
  if (!is.null(seed)) set.seed(seed)

  # Create local copies of variable names for data.table scoping
  unit_col <- unit
  group_col <- group
  time_col <- time
  outcome_col <- outcome
  engine <- noise_spec$engine

  # Count/binomial obs model: skip noise calibration entirely (stochasticity via draw in caller)
  use_count_obs <- noise_spec$obs_model %in% c("poisson", "binomial")

  # Get key parameters
  groups <- sort(unique(df[[group_col]]))   # NA (never-treated) dropped: not a cohort
  max_year <- max(df[[time_col]])

  # Make a copy to store counterfactuals
  df_new <- data.table::as.data.table(df)
  df_new[, counterfactual := get(outcome_col)]
  if (use_count_obs) df_new[, cf_mean_rate := NA_real_]

  # Calculate rel_pass if not present
  if (!"rel_pass" %in% names(df_new)) {
    df_new[, rel_pass := get(time_col) - get(group_col)]
  }

  # --- HARMONIZED control counterfactual: read did's OWN control-side. ---
  # This replaces the legacy hand-rolled control long-difference regression, which
  # only matched did::att_gt on balanced panels and left a non-zero CS/CS null bias
  # on ragged ones. The DGP now inherits exactly whatever control group + estimand
  # the estimator uses (incl. late-adopter / not-yet-treated controls), by design.
  delta_dt <- .cs_delta_control(df_new, unit_col, group_col, time_col, outcome_col,
                                controls, control_group, est_method, base_period,
                                allow_unbalanced_panel)
  data.table::setkeyv(delta_dt, c("g", "t"))
  # Per-cohort fallback (mean estimable delta) for treated post cells that did does
  # NOT estimate (structurally: late periods / last cohort, or ragged coverage).
  # Such cells are INERT for the estimator's ATT (it can't estimate them either), so
  # the fallback only keeps the simulated panel complete (no NA outcomes downstream).
  cohort_fb <- delta_dt[!is.na(delta_control), .(fb = mean(delta_control)), by = g]
  n_estimable <- nrow(delta_dt[!is.na(delta_control)])
  global_fb <- if (n_estimable > 0) {
    mean(delta_dt$delta_control, na.rm = TRUE)   # cohort has NO estimable cell at all
  } else NA_real_
  # No usable (g,t) cell at all: no counterfactual can be built, so the returned
  # outcomes would silently equal the observed data (invalid for a power DGP). Warn
  # loudly rather than fail silently. Usually means no valid control group exists
  # (e.g. a single treated cohort, or no not-yet-treated / never-treated units).
  if (n_estimable == 0) {
    warning("staggeredpower (CS DGP): did::att_gt estimated no usable (g, t) cells for ",
            "this panel; no counterfactual could be built and the returned outcomes equal ",
            "the observed data. Downstream power/rejection numbers will be invalid.",
            call. = FALSE)
  }

  # NOTE: group/time filters below use base-R column extraction (df[[group_col]])
  # rather than data.table NSE (get(group_col) == gv). The loop scalar must never be
  # resolvable as a column: if the caller's group column is literally named "g", a
  # `get(group_col) == g` filter silently becomes `g_col == g_col` (all TRUE).

  # --- Pre-compute calibration for AR(1) engines (unchanged; independent of mean) ---
  shock_lookup <- NULL
  if (!use_count_obs && engine %in% c("ar1", "ar1_common", "ar1_anchored")) {
    noise_calib <- calibrate_noise_cs(df, unit_col, group_col, time_col, outcome_col,
                                       controls, noise_spec)
    treated_units_by_cohort <- list()
    max_rp_by_cohort <- list()
    for (gv in groups) {
      g_char <- as.character(gv)
      in_g <- df[[group_col]] == gv & !is.na(df[[group_col]])   # NA & FALSE -> FALSE (no NA leak)
      treated_units_by_cohort[[g_char]] <- unique(df[[unit_col]][in_g])
      max_rp_by_cohort[[g_char]] <- max(df_new$rel_pass[df_new[[group_col]] == gv], na.rm = TRUE)
    }
    shock_dt <- draw_noise_cs(noise_calib, treated_units_by_cohort, max_rp_by_cohort)
    data.table::setkeyv(shock_dt, c("unit", "cohort", "rp"))
    shock_lookup <- shock_dt
  }

  # --- Main (g, rp) loop: mu_hat(i,t) = Y_{i,g-1} + Delta_control(g,t) ---
  for (gv in groups) {
    rp_vals <- df_new$rel_pass[df_new[[group_col]] == gv]
    max_rel_pass <- if (all(is.na(rp_vals))) -Inf else max(rp_vals, na.rm = TRUE)
    # Skip cohorts with NO observed post-period (max rel_pass < 0): `0:max_rel_pass`
    # would otherwise descend into negative rp (curr_time = gv-1 = pre-period, which
    # would overwrite pre-treatment outcomes) or, at -Inf, error on `0:-Inf`.
    if (!is.finite(max_rel_pass) || max_rel_pass < 0) next

    for (rp in 0:max_rel_pass) {
      curr_time <- gv + rp
      if (curr_time > max_year) next
      pre_time <- gv - 1

      g_cur <- gv; t_cur <- curr_time
      dc_row <- delta_dt[g == g_cur & t == t_cur]
      if (nrow(dc_row) > 0 && !is.na(dc_row$delta_control[1])) {
        dc <- dc_row$delta_control[1]              # did's own control-side (exact)
      } else {
        fb <- cohort_fb$fb[cohort_fb$g == g_cur]   # inert fallback for unestimated cells
        if (length(fb) == 0 || is.na(fb)) fb <- global_fb   # cohort has no cell at all
        if (length(fb) == 0 || is.na(fb)) next     # did estimated nothing on this panel
        dc <- fb
      }

      # Treated units in cohort gv present at BOTH the base year (gv-1) and curr_time
      in_cohort <- df_new[[group_col]] == gv & !is.na(df_new[[group_col]])
      pre <- df_new[in_cohort & df_new[[time_col]] == pre_time,
                    .(cf_u = get(unit_col), cf_anchor = get(outcome_col))]
      post_units <- df_new[[unit_col]][in_cohort & df_new[[time_col]] == curr_time]
      pre <- pre[cf_u %in% post_units & !is.na(cf_anchor)]
      if (nrow(pre) == 0) next

      mu_hat <- pre$cf_anchor + dc   # common control shift added to each unit's own anchor

      # Apply noise based on engine and obs_model
      if (use_count_obs || engine == "none") {
        vals <- mu_hat
      } else if (engine %in% c("ar1", "ar1_common", "ar1_anchored")) {
        eps_vals <- vapply(pre$cf_u, function(uid) {
          mr <- shock_lookup[list(uid, gv, rp)]
          if (nrow(mr) > 0 && !is.na(mr$eps_ld[1])) mr$eps_ld[1] else 0
        }, numeric(1))
        vals <- mu_hat + eps_vals
      } else if (engine == "iid") {
        rsd <- .cs_control_resid_sd(df, unit_col, group_col, time_col, outcome_col,
                                    controls, curr_time, pre_time, max_year)
        vals <- mu_hat + stats::rnorm(length(mu_hat), 0, rsd)
      } else {
        vals <- mu_hat
      }

      # Per-unit keyed assignment (robust to ragged treated coverage)
      assign_dt <- data.table::data.table(cf_u = pre$cf_u, cf_tt = curr_time, cf_val = vals)
      on_spec <- stats::setNames(c("cf_u", "cf_tt"), c(unit_col, time_col))
      df_new[assign_dt, on = on_spec, counterfactual := i.cf_val]
      if (use_count_obs) df_new[assign_dt, on = on_spec, cf_mean_rate := i.cf_val]
    }
  }

  return(df_new)
}


#' Residual SD of control long-differences for the legacy iid noise engine
#' @noRd
.cs_control_resid_sd <- function(df, unit_col, group_col, time_col, outcome_col,
                                 controls, curr_time, pre_time, max_year) {
  control_data <- df[(get(group_col) > curr_time & get(group_col) <= max_year) |
                       is.na(get(group_col))]
  cp <- control_data[get(time_col) == pre_time]
  cq <- control_data[get(time_col) == curr_time]
  cc <- merge(cp, cq, by = c(unit_col), suffixes = c("_pre", "_post"))
  if (nrow(cc) == 0) return(0)
  dy <- cc[[paste0(outcome_col, "_post")]] - cc[[paste0(outcome_col, "_pre")]]
  if (!is.null(controls) && length(controls) > 0) {
    X <- as.matrix(cbind(1, cc[, paste0(controls, "_pre"), with = FALSE]))
  } else {
    X <- as.matrix(rep(1, length(dy)))
  }
  fit <- stats::lm.fit(x = X, y = dy)
  dfree <- length(dy) - ncol(X)
  sqrt(sum(fit$residuals^2) / max(dfree, 1))
}


#' Generate counterfactual outcomes using Poisson approach (multiplicative PT)
#'
#' @description
#' This function implements a Poisson-based approach for difference-in-differences with
#' staggered adoption. It estimates a Poisson fixed effects model using only untreated
#' observations and generates counterfactual outcomes for treated observations on the
#' log-rate scale, then back-transforms to counts or rates.
#'
#' This implements the multiplicative parallel trends assumption (Wooldridge 2023):
#' \eqn{E[Y_t(0)] / E[Y_{t-1}(0)]} is constant across treatment groups.
#'
#' @param df A data.table containing the panel data
#' @param unit Character string specifying the column name for unit identifiers
#' @param group Character string specifying the column name for treatment group/cohort
#' @param time Character string specifying the column name for time periods
#' @param outcome Character string specifying the column name for the outcome variable
#' @param controls Character vector. Names of control variables (default: NULL)
#' @param seed Numeric. Random seed for reproducibility (default: NULL)
#' @param pop_var Character. Name of population variable (required)
#' @param outcome_type Character. 'count' or 'rate' (default: 'rate')
#'
#' @return A data.table with an additional column 'counterfactual' containing:
#'   - Actual outcomes for untreated observations
#'   - Imputed counterfactual outcomes for treated observations
#'
#' @details
#' The function follows these steps:
#' 1. Converts rates to counts if needed (count = rate * pop / 100000)
#' 2. Splits data into treated and untreated observations
#' 3. Estimates Poisson fixed effects model on untreated observations:
#'    \eqn{E[count | untreated] = exp(alpha_i + gamma_t + X*beta)} with log(pop) offset
#' 4. Uses these estimates to predict counterfactual log-rates for treated observations
#' 5. Adds Poisson noise (using rpois) to generate stochastic counterfactuals
#' 6. Converts back to rate scale if needed
#'
#' @keywords internal
enforce_PTA_poisson <- function(df, unit, group, time, outcome,
                                 controls = NULL, seed = NULL,
                                 pop_var = NULL, outcome_type = "rate",
                                 trend_type = "common", trend_order = 1L,
                                 noise_spec = NULL) {
  noise_spec <- normalize_noise_spec(noise_spec)

  # Constants
  RATE_SCALE <- 100000

  if (!is.null(seed)) set.seed(seed)

  # Validate pop_var

  if (is.null(pop_var)) {
    stop("enforce_PTA_poisson requires pop_var to be specified")
  }

  df <- data.table::as.data.table(df)

  # Validate population variable
  if (!(pop_var %in% names(df))) {
    stop(sprintf("pop_var '%s' not found in data", pop_var))
  }
  if (any(df[[pop_var]] <= 0, na.rm = TRUE)) {
    stop("pop_var contains non-positive values; cannot use Poisson model")
  }

  # Create local copies of variable names for data.table scoping
  # This avoids conflicts when parameter names match column names
  group_col <- group
  time_col <- time
  unit_col <- unit
  outcome_col <- outcome

  # Convert to count if input is rate
  if (outcome_type == "rate") {
    df[, .pois_count := get(outcome_col) * get(pop_var) / RATE_SCALE]
    working_outcome <- ".pois_count"
  } else {
    working_outcome <- outcome_col
  }

  # Create log(pop) offset
  df[, .log_pop := log(get(pop_var))]

  # Robust treated flag: TRUE if treated unit and at or after treatment time
  # Uses >= to match standard Poisson GLM definition (treatment starts at year_passed)
  df[, treated := !is.na(get(group_col)) & get(time_col) >= get(group_col)]

  # Keep untreated observations for Poisson FE estimation
  df_untreated <- df[treated == FALSE | is.na(treated)]

  # Check we have enough untreated observations
  if (nrow(df_untreated) < 10) {
    stop("Too few untreated observations to fit Poisson model")
  }

  # Create time-centered variable and cohort-specific trend variables for cohort_trend
  # We use separate numeric trend variables (one per cohort × polynomial degree) to avoid NAs
  cohort_trend_vars <- character(0)  # Track created trend variable names
  if (trend_type == "cohort_trend") {
    min_time <- min(df[[time_col]], na.rm = TRUE)
    df[, .time_centered := get(time_col) - min_time]
    df_untreated[, .time_centered := get(time_col) - min_time]

    # Get unique treatment cohorts (excluding never-treated which have NA group)
    treated_cohorts <- sort(unique(df[!is.na(get(group_col))][[group_col]]))

    # Create trend variables for each cohort × polynomial degree
    # Never-treated units get 0 for all trend variables (their trend is captured by common time FE)
    for (cohort in treated_cohorts) {
      for (deg in seq_len(trend_order)) {
        var_name <- paste0(".trend_c", cohort, "_d", deg)
        cohort_trend_vars <- c(cohort_trend_vars, var_name)

        # Value = time_centered^deg if unit belongs to this cohort, 0 otherwise
        df[, (var_name) := data.table::fifelse(get(group_col) == cohort, .time_centered^deg, 0, na = 0)]
        df_untreated[, (var_name) := data.table::fifelse(get(group_col) == cohort, .time_centered^deg, 0, na = 0)]
      }
    }
  }

  # Build Poisson FE formula with offset
  # Using fixest::fepois for Poisson with fixed effects
  if (trend_type == "common") {
    # Standard Poisson TWFE
    if (!is.null(controls) && length(controls) > 0) {
      control_terms <- paste(controls, collapse = " + ")
      fe_formula <- stats::as.formula(
        paste0(working_outcome, " ~ ", control_terms, " + offset(.log_pop) | ", unit_col, " + ", time_col)
      )
    } else {
      fe_formula <- stats::as.formula(
        paste0(working_outcome, " ~ offset(.log_pop) | ", unit_col, " + ", time_col)
      )
    }
  } else if (trend_type == "cohort_trend") {
    # Cohort-specific polynomial trends using pre-created trend variables
    # Each variable is cohort-specific: .trend_c{cohort}_d{degree}
    trend_term <- paste(cohort_trend_vars, collapse = " + ")
    if (!is.null(controls) && length(controls) > 0) {
      control_terms <- paste(controls, collapse = " + ")
      fe_formula <- stats::as.formula(
        paste0(working_outcome, " ~ ", trend_term, " + ", control_terms, " + offset(.log_pop) | ",
               unit_col, " + ", time_col)
      )
    } else {
      fe_formula <- stats::as.formula(
        paste0(working_outcome, " ~ ", trend_term, " + offset(.log_pop) | ", unit_col, " + ", time_col)
      )
    }
  }

  # Fit Poisson model
  mod_pois <- tryCatch({
    fixest::fepois(fe_formula, data = df_untreated)
  }, error = function(e) {
    stop(sprintf("Poisson fixed effects model failed: %s", e$message))
  })

  # Calibrate noise engine (log scale)
  noise_calib <- calibrate_noise_poisson(mod_pois, df_untreated, working_outcome,
                                          pop_var, unit_col, time_col, noise_spec)

  # Extract fixed effects
  unit_names <- names(fixest::fixef(mod_pois)[[unit_col]])
  if (is.numeric(df[[unit_col]])) {
    unit_names <- as.numeric(unit_names)
  }
  unit_effects <- data.table::data.table(
    unit = unit_names,
    unit_effect = as.numeric(fixest::fixef(mod_pois)[[unit_col]])
  )
  data.table::setnames(unit_effects, "unit", unit_col)

  time_names <- names(fixest::fixef(mod_pois)[[time_col]])
  if (is.numeric(df[[time_col]])) {
    time_names <- as.numeric(time_names)
  }
  time_effects <- data.table::data.table(
    time = time_names,
    time_effect = as.numeric(fixest::fixef(mod_pois)[[time_col]])
  )
  data.table::setnames(time_effects, "time", time_col)

  # Merge effects
  df[unit_effects, on = unit_col, unit_effect := i.unit_effect]
  df[time_effects, on = time_col, time_effect := i.time_effect]

  # Initialize counterfactual as observed
  df[, counterfactual := get(outcome_col)]

  # Compute deterministic mu_log_hat for treated observations
  # mu_log_hat = alpha_i + gamma_t + [trend] + X*beta  (log scale, without pop offset)
  if (trend_type == "common") {
    if (!is.null(controls) && length(controls) > 0) {
      control_coefs <- stats::coef(mod_pois)[controls]
      control_coefs[is.na(control_coefs)] <- 0

      df[treated == TRUE, control_effect :=
           Reduce(`+`, Map(function(x, y) get(x) * y, controls, control_coefs))]

      df[treated == TRUE, .mu_log_hat := unit_effect + time_effect + control_effect]
      df[, control_effect := NULL]
    } else {
      df[treated == TRUE, .mu_log_hat := unit_effect + time_effect]
    }
  } else if (trend_type == "cohort_trend") {
    all_coefs <- stats::coef(mod_pois)

    df[treated == TRUE, .trend_effect := {
      effect <- numeric(.N)
      for (var_name in cohort_trend_vars) {
        if (var_name %in% names(all_coefs) && !is.na(all_coefs[var_name])) {
          effect <- effect + get(var_name) * all_coefs[var_name]
        }
      }
      effect
    }]

    if (!is.null(controls) && length(controls) > 0) {
      control_coefs <- all_coefs[controls]
      control_coefs[is.na(control_coefs)] <- 0

      df[treated == TRUE, .control_effect :=
           Reduce(`+`, Map(function(x, y) get(x) * y, controls, control_coefs))]

      df[treated == TRUE, .mu_log_hat := unit_effect + time_effect + .trend_effect + .control_effect]
      df[, .control_effect := NULL]
    } else {
      df[treated == TRUE, .mu_log_hat := unit_effect + time_effect + .trend_effect]
    }
  }

  # Draw log-scale noise and apply
  treated_rows <- df[treated == TRUE]
  if (nrow(treated_rows) > 0) {
    eps_dt <- draw_noise(noise_calib,
                          units = treated_rows[[unit_col]],
                          times = treated_rows[[time_col]])
    eps_dt_named <- data.table::copy(eps_dt)
    data.table::setnames(eps_dt_named, c("unit", "time"), c(unit_col, time_col))
    df[eps_dt_named, on = c(unit_col, time_col), .noise_eps := i.eps]
    df[is.na(.noise_eps), .noise_eps := 0]

    # Apply noise on log scale, then transform to counts
    df[treated == TRUE, .log_rate_cf := .mu_log_hat + .noise_eps]
    df[treated == TRUE, .lambda := exp(.log_rate_cf) * get(pop_var)]

    # Handle edge cases
    df[treated == TRUE & (is.na(.lambda) | .lambda < 0), .lambda := 0]

    # Observation model
    if (noise_spec$obs_model == "poisson") {
      df[treated == TRUE, .counterfactual_count := stats::rpois(.N, .lambda)]
    } else {
      # deterministic: use expected value
      df[treated == TRUE, .counterfactual_count := .lambda]
    }

    # Convert to rate scale if needed
    if (outcome_type == "rate") {
      df[treated == TRUE, counterfactual := .counterfactual_count / get(pop_var) * RATE_SCALE]
    } else {
      df[treated == TRUE, counterfactual := .counterfactual_count]
    }
  }

  # Clean up temporary columns
  temp_cols <- c(".pois_count", ".log_pop", "treated", "unit_effect", "time_effect",
                 ".mu_log_hat", ".noise_eps", ".log_rate_cf", ".lambda", ".counterfactual_count",
                 ".time_centered", ".trend_effect", cohort_trend_vars)
  existing_temp <- intersect(temp_cols, names(df))
  if (length(existing_temp) > 0) {
    df[, (existing_temp) := NULL]
  }

  return(df)
}
