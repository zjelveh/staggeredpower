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
#' @param rel_pass_var Character. Name of relative time column (e.g., 'rel_pass')
#' @param outcome Character. Name of outcome column 
#' @param pta_type Character. Control assumption: 'cs' or 'imputation'
#' @param enforce_type Character. Method for enforcing PTA: 'simple', 'realistic', or 'simple_correlated'
#'
#' @return A list containing:
#' \describe{
#'   \item{df_new}{Data.frame with enforced parallel trends}
#'   \item{stats}{Data.frame with enforcement statistics}
#' }
#'
#' @examples
#' \dontrun{
#' data <- prepare_panel_data(your_data)
#' pta_results <- enforce_PTA(
#'   df = data,
#'   unit = "county_name",
#'   group = "year_passed",
#'   time = "year",
#'   outcome = "outcome_var",
#'   period = "pre_year",
#'   cohort = "notyettreated"
#' )
#' }
#'
#' @importFrom data.table data.table setnames :=
#' @export
enforce_PTA <- function(df, unit, group, time, outcome,
                        controls = NULL,
                        method = c("imputation", "CS"),
                        seed = NULL) {
  method <- match.arg(method)
  if (method == "imputation") {
    enforce_PTA_imputation(df, unit, group, time, outcome, controls, seed)
  } else {
    enforce_PTA_CS(df, unit, group, time, outcome, controls, seed)
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
enforce_PTA_imputation <- function(df, unit, group, time, outcome, controls = NULL, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  df <- copy(df)
  
  # Robust treated flag: TRUE if treated unit and post-treatment time
  df[, treated := !is.na(get(group)) & get(time) > get(group)]
  
  # Keep untreated observations for FE estimation
  df_untreated <- df[treated == FALSE | is.na(treated)]
  
  # Build FE formula
  if (!is.null(controls) && length(controls) > 0) {
    control_terms <- paste(controls, collapse = " + ")
    fe_formula <- as.formula(paste0(outcome, " ~ ", control_terms, " | ", unit, " + ", time))
  } else {
    fe_formula <- as.formula(paste0(outcome, " ~ 1 | ", unit, " + ", time))
  }
  
  mod_fe <- feols(fe_formula, data = df_untreated)
  resid_sd <- sigma(mod_fe)
  
  # Extract fixed effects
  unit_effects <- data.table(
    unit = names(fixef(mod_fe)[[unit]]),
    unit_effect = as.numeric(fixef(mod_fe)[[unit]])
  )
  time_effects <- data.table(
    time = names(fixef(mod_fe)[[time]]),
    time_effect = as.numeric(fixef(mod_fe)[[time]])
  )
  
  # Merge effects
  df[unit_effects, on = unit, unit_effect := i.unit_effect]
  df[time_effects, on = time, time_effect := i.time_effect]
  
  # Initialize counterfactual as observed
  df[, counterfactual := get(outcome)]
  
  # Predict counterfactuals for treated
  if (!is.null(controls) && length(controls) > 0) {
    control_effects <- coef(mod_fe)[controls]
    control_effects[is.na(control_effects)] <- 0
    
    df[treated == TRUE, control_effect := 
         Reduce(`+`, Map(function(x, y) get(x) * y, controls, control_effects))]
    
    df[treated == TRUE, counterfactual := unit_effect + time_effect + control_effect]
    df[, control_effect := NULL]
  } else {
    df[treated == TRUE, counterfactual := unit_effect + time_effect]
  }
  
  return(df)
}




enforce_PTA_CS <- function(df, unit, group, time, outcome, controls = NULL, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  df <- copy(df)
  df[, treated := !is.na(get(group)) & get(time) > get(group)]
  
  df_untreated <- df[treated == FALSE | is.na(treated)]
  
  # Fit pre/post regression for untreated
  pre_period <- min(df_untreated[[time]])
  post_period <- max(df_untreated[[time]])
  df_untreated[, delta_y := fifelse(get(time) == post_period, get(outcome), NA) -
                                 fifelse(get(time) == pre_period, get(outcome), NA)]
  df_untreated <- df_untreated[!is.na(delta_y)]
  
  if (!is.null(controls) && length(controls) > 0) {
    control_terms <- paste(controls, collapse = " + ")
    reg_formula <- as.formula(paste0("delta_y ~ ", control_terms))
  } else {
    reg_formula <- as.formula("delta_y ~ 1")
  }
  
  reg_model <- lm(reg_formula, data = df_untreated)
  
  resid_sd <- if (reg_model$df.residual > 0) {
    sqrt(sum(reg_model$residuals^2) / reg_model$df.residual)
  } else {
    sd(df_untreated$delta_y, na.rm = TRUE)
  }
  
  # Predict counterfactuals for treated units
  df[, counterfactual := get(outcome)]
  if (!is.null(controls) && length(controls) > 0) {
    control_effects <- coef(reg_model)[controls]
    control_effects[is.na(control_effects)] <- 0
    df[treated == TRUE, control_effect := 
         Reduce(`+`, Map(function(x, y) get(x) * y, controls, control_effects))]
    df[treated == TRUE, counterfactual := get(outcome) - control_effect]
    df[, control_effect := NULL]
  }
  
  return(df)
}
