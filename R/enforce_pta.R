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
#' \dontrun{
#' # Linear approach (additive PT)
#' pta_results <- enforce_PTA(
#'   df = data,
#'   unit = "county_name",
#'   group = "year_passed",
#'   time = "year",
#'   outcome = "rate_per_100k",
#'   method = "imputation"
#' )
#'
#' # Poisson approach (multiplicative PT)
#' pta_results <- enforce_PTA(
#'   df = data,
#'   unit = "county_name",
#'   group = "year_passed",
#'   time = "year",
#'   outcome = "count",
#'   method = "poisson",
#'   pop_var = "population",
#'   outcome_type = "count"
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
                        trend_order = 1L) {
  method <- match.arg(method)
  trend_type <- match.arg(trend_type)
  trend_order <- as.integer(trend_order)

  # Validate trend_order

  if (trend_type == "cohort_trend" && trend_order < 1L) {
    stop("trend_order must be >= 1 for cohort_trend")
  }

  if (method == "imputation") {
    enforce_PTA_imputation(df, unit, group, time, outcome, controls, seed,
                           trend_type, trend_order)
  } else if (method == "CS") {
    # CS already has cohort-specific trends implicitly via not-yet-treated controls
    # trend_type and trend_order are ignored for CS method
    enforce_PTA_CS(df, unit, group, time, outcome, controls, seed)
  } else if (method == "poisson") {
    enforce_PTA_poisson(df, unit, group, time, outcome, controls, seed,
                        pop_var, outcome_type, trend_type, trend_order)
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
enforce_PTA_imputation <- function(df, unit, group, time, outcome, controls = NULL, seed = NULL,
                                   trend_type = "common", trend_order = 1L) {
  if (!is.null(seed)) set.seed(seed)

  df <- copy(df)

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

  mod_fe <- feols(fe_formula, data = df_untreated)
  resid_sd <- sigma(mod_fe)

  # Extract fixed effects
  # Try to preserve the original data type
  unit_names <- names(fixef(mod_fe)[[unit_col]])
  if(is.numeric(df[[unit_col]])) {
    unit_names <- as.numeric(unit_names)
  }
  unit_effects <- data.table(
    unit = unit_names,
    unit_effect = as.numeric(fixef(mod_fe)[[unit_col]])
  )
  setnames(unit_effects, "unit", unit_col)  # Rename to match df column name

  time_names <- names(fixef(mod_fe)[[time_col]])
  if(is.numeric(df[[time_col]])) {
    time_names <- as.numeric(time_names)
  }
  time_effects <- data.table(
    time = time_names,
    time_effect = as.numeric(fixef(mod_fe)[[time_col]])
  )
  setnames(time_effects, "time", time_col)  # Rename to match df column name

  # Merge effects
  df[unit_effects, on = unit_col, unit_effect := i.unit_effect]
  df[time_effects, on = time_col, time_effect := i.time_effect]

  # Initialize counterfactual as observed
  df[, counterfactual := get(outcome_col)]

  # Predict counterfactuals for treated
  if (trend_type == "common") {
    # Standard approach: alpha_i + gamma_t + X*beta + noise
    if (!is.null(controls) && length(controls) > 0) {
      control_effects <- coef(mod_fe)[controls]
      control_effects[is.na(control_effects)] <- 0

      df[treated == TRUE, control_effect :=
           Reduce(`+`, Map(function(x, y) get(x) * y, controls, control_effects))]

      df[treated == TRUE, counterfactual := unit_effect + time_effect + control_effect +
           rnorm(.N, mean=0, sd=resid_sd)]
      df[, control_effect := NULL]
    } else {
      df[treated == TRUE, counterfactual := unit_effect + time_effect +
           rnorm(.N, mean=0, sd=resid_sd)]
    }
  } else if (trend_type == "cohort_trend") {
    # Extract all coefficients from model
    all_coefs <- coef(mod_fe)

    # Calculate trend effect for each treated observation
    # Use the pre-created trend variables (.trend_c{cohort}_d{deg})
    # Trend effect = sum over all trend vars of (coef * value)
    df[treated == TRUE, .trend_effect := {
      effect <- numeric(.N)
      for (var_name in cohort_trend_vars) {
        if (var_name %in% names(all_coefs) && !is.na(all_coefs[var_name])) {
          effect <- effect + get(var_name) * all_coefs[var_name]
        }
      }
      effect
    }]

    # Add control effects if present and compute counterfactual
    if (!is.null(controls) && length(controls) > 0) {
      control_coefs <- all_coefs[controls]
      control_coefs[is.na(control_coefs)] <- 0

      df[treated == TRUE, .control_effect :=
           Reduce(`+`, Map(function(x, y) get(x) * y, controls, control_coefs))]

      df[treated == TRUE, counterfactual :=
           unit_effect + time_effect + .trend_effect + .control_effect +
           rnorm(.N, mean=0, sd=resid_sd)]
      df[, .control_effect := NULL]
    } else {
      df[treated == TRUE, counterfactual :=
           unit_effect + time_effect + .trend_effect +
           rnorm(.N, mean=0, sd=resid_sd)]
    }

    # Cleanup cohort_trend-specific columns
    temp_cols <- c(".time_centered", ".trend_effect", cohort_trend_vars)
    existing_cols <- intersect(temp_cols, names(df))
    if (length(existing_cols) > 0) {
      df[, (existing_cols) := NULL]
    }
  }

  # Cleanup common columns
  df[, c("unit_effect", "time_effect", "treated") := NULL]

  return(df)
}




enforce_PTA_CS <- function(df, unit, group, time, outcome, controls = NULL, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  # Create local copies of variable names for data.table scoping
  unit_col <- unit
  group_col <- group
  time_col <- time
  outcome_col <- outcome

  # Get key parameters
  groups = sort(unique(df[[group_col]]))
  max_year = max(df[[time_col]])

  # Make a copy to store counterfactuals
  df_new = copy(df)
  df_new[, counterfactual := get(outcome_col)]

  # Calculate rel_pass_var if not present
  if (!"rel_pass" %in% names(df_new)) {
    df_new[, rel_pass := get(time_col) - get(group_col)]
  }

  # Iterate over each group
  for(g in groups) {
    # Get the max relative time for this group
    max_rel_pass = max(df_new[get(group_col)==g]$rel_pass, na.rm=TRUE)

    # For each relative period after treatment
    for(rp in 0:max_rel_pass) {

      curr_time = g + rp
      if(curr_time <= max_year) {
        pre_time = g - 1

        # Get control group data - those not yet treated at curr_time
        # IMPORTANT: Also exclude units with year_passed > max_year, as did::att_gt
        # excludes units whose treatment date is beyond the data range.
        # This ensures the control group matches what did::att_gt will use.
        control_data = df[(get(group_col) > curr_time & get(group_col) <= max_year) | is.na(get(group_col))]

        # Get pre/post periods for controls
        control_pre = control_data[get(time_col) == pre_time]
        control_post = control_data[get(time_col) == curr_time]

        # Merge pre/post for controls to calculate changes
        control_changes = merge(control_pre, control_post,
                                by=c(unit_col),
                                suffixes=c("_pre", "_post"))

        control_changes[, delta_y := get(paste0(outcome_col, "_post")) -
                          get(paste0(outcome_col, "_pre"))]

        # Use pre-period controls for regression
        if(!is.null(controls) && length(controls) > 0) {
          # Use the _pre version of controls from merge
          X_control = as.matrix(cbind(1, control_changes[, paste0(controls, "_pre"), with=FALSE]))
        } else {
          X_control = as.matrix(rep(1, nrow(control_changes)))
        }

        if(length(control_changes$delta_y)>0){
          # Fit regression of changes on covariates for control group
          reg_model = fastglm::fastglm(
            x = X_control,
            y = control_changes$delta_y,
            family = stats::gaussian(link = "identity")
          )

          if(reg_model$df.residual==0){
            resid_sd <- sqrt(sum(reg_model$residuals^2) / 1)
          } else{
            resid_sd <- sqrt(sum(reg_model$residuals^2) / reg_model$df.residual)
          }

          # Get treated group data at pre-treatment period
          treated_pre = df[get(group_col) == g & get(time_col) == pre_time]

          # Use pre-period controls for treated units
          if(!is.null(controls) && length(controls) > 0) {
            X_treated = as.matrix(cbind(1, treated_pre[, ..controls]))
          } else {
            X_treated = as.matrix(rep(1, nrow(treated_pre)))
          }

          # Predict counterfactual changes
          predicted_changes = as.vector(X_treated %*% reg_model$coefficients)

          # Calculate counterfactual outcomes
          counterfactuals = treated_pre[[outcome_col]] + predicted_changes +
            rnorm(length(predicted_changes), mean=0, sd=resid_sd)

          # Update counterfactual values
          df_new[get(group_col) == g & get(time_col) == curr_time,
                 counterfactual := counterfactuals]
        }
      }
    }
  }

  return(df_new)
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
#' E[Y_t(0)] / E[Y_{t-1}(0)] is constant across treatment groups.
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
#'    E[count | untreated] = exp(alpha_i + gamma_t + X*beta) with log(pop) offset
#' 4. Uses these estimates to predict counterfactual log-rates for treated observations
#' 5. Adds Poisson noise (using rpois) to generate stochastic counterfactuals
#' 6. Converts back to rate scale if needed
#'
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
enforce_PTA_poisson <- function(df, unit, group, time, outcome,
                                 controls = NULL, seed = NULL,
                                 pop_var = NULL, outcome_type = "rate",
                                 trend_type = "common", trend_order = 1L) {

  # Constants
  RATE_SCALE <- 100000

  if (!is.null(seed)) set.seed(seed)

  # Validate pop_var

  if (is.null(pop_var)) {
    stop("enforce_PTA_poisson requires pop_var to be specified")
  }

  df <- data.table::copy(df)

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

  # Predict counterfactual counts for treated observations
  # E[count] = exp(alpha_i + gamma_t + [trend] + X*beta + log(pop))
  #          = pop * exp(alpha_i + gamma_t + [trend] + X*beta)
  if (trend_type == "common") {
    # Standard approach without cohort-specific trends
    if (!is.null(controls) && length(controls) > 0) {
      control_coefs <- stats::coef(mod_pois)[controls]
      control_coefs[is.na(control_coefs)] <- 0

      # Calculate control effect for treated obs
      df[treated == TRUE, control_effect :=
           Reduce(`+`, Map(function(x, y) get(x) * y, controls, control_coefs))]

      # Predicted rate on log scale (without pop offset)
      df[treated == TRUE, .log_rate := unit_effect + time_effect + control_effect]
      df[, control_effect := NULL]
    } else {
      df[treated == TRUE, .log_rate := unit_effect + time_effect]
    }
  } else if (trend_type == "cohort_trend") {
    # Extract all coefficients from model
    all_coefs <- stats::coef(mod_pois)

    # Calculate trend effect for each treated observation (on log scale)
    # Use the pre-created trend variables (.trend_c{cohort}_d{deg})
    df[treated == TRUE, .trend_effect := {
      effect <- numeric(.N)
      for (var_name in cohort_trend_vars) {
        if (var_name %in% names(all_coefs) && !is.na(all_coefs[var_name])) {
          effect <- effect + get(var_name) * all_coefs[var_name]
        }
      }
      effect
    }]

    # Add control effects if present
    if (!is.null(controls) && length(controls) > 0) {
      control_coefs <- all_coefs[controls]
      control_coefs[is.na(control_coefs)] <- 0

      df[treated == TRUE, .control_effect :=
           Reduce(`+`, Map(function(x, y) get(x) * y, controls, control_coefs))]

      df[treated == TRUE, .log_rate := unit_effect + time_effect + .trend_effect + .control_effect]
      df[, .control_effect := NULL]
    } else {
      df[treated == TRUE, .log_rate := unit_effect + time_effect + .trend_effect]
    }
  }

  # Generate counterfactual counts using Poisson distribution
  # This preserves the discrete, non-negative nature of counts
  df[treated == TRUE, .lambda := exp(.log_rate) * get(pop_var)]

  # Handle case where lambda is extremely small or NA
  df[treated == TRUE & (is.na(.lambda) | .lambda < 0), .lambda := 0]

  # Draw from Poisson
  df[treated == TRUE, .counterfactual_count := stats::rpois(.N, .lambda)]

  # Convert back to rate scale if needed
  if (outcome_type == "rate") {
    df[treated == TRUE, counterfactual := .counterfactual_count / get(pop_var) * RATE_SCALE]
  } else {
    df[treated == TRUE, counterfactual := .counterfactual_count]
  }

  # Clean up temporary columns
  temp_cols <- c(".pois_count", ".log_pop", "treated", "unit_effect", "time_effect",
                 ".log_rate", ".lambda", ".counterfactual_count",
                 ".time_centered", ".trend_effect", cohort_trend_vars)
  existing_temp <- intersect(temp_cols, names(df))
  if (length(existing_temp) > 0) {
    df[, (existing_temp) := NULL]
  }

  return(df)
}
