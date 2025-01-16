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
#' @param max_year Numeric. Maximum year to include in analysis
#' @param pta_type Character. Control assumption: 'cs' or 'imputation'
#' @param enforce_type Character. Method for enforcing PTA: 'simple', 'realistic', or 'simple_correlated'
#' @param rho_hat Numeric. AR(1) coefficient for realistic enforcement
#' @param sd_resid Numeric. Residual standard deviation for realistic enforcement
#' @param time_fe Data.frame. Time fixed effects
#' @param unit_fe Data.frame. Unit fixed effects
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
enforce_PTA <- function(df,
                        unit = 'county_name',
                        group = 'year_passed', 
                        time = 'year',
                        rel_pass_var = 'rel_pass',
                        outcome = NULL,
                        pta_type = 'cs',
                        enforce_type = NULL) {
  
  if(pta_type=='imputation'){
    df_cf = enforce_PTA_imputation(df, unit, group, time, 
                                   outcome, enforce_type)
  }
  if(pta_type=='cs'){
    df_cf = enforce_PTA_CS(df, unit, group, time, rel_pass_var,
                           outcome, enforce_type)
  }
  
  
  df_cf[, bound_error:= ifelse(counterfactual<0, 1, 0)]
  df_cf[bound_error==1, counterfactual:=0]
  df_cf[, na_error:=ifelse(is.na(counterfactual), 1, 0)]
  return(df_cf)
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
enforce_PTA_imputation = function(df, unit, group, time, outcome, controls){
  # Split into pre/post treatment periods  
  df[, treated := get(time) > get(group)]
  df_untreated = df[treated==FALSE]
  
  # Estimate FEs with controls if provided
  if(length(controls) > 0){
    # Create formula with all controls
    control_terms = paste(controls, collapse = " + ")
    fe_formula = as.formula(paste0(outcome, " ~ ", control_terms, " | ", unit, " + ", time))
  } else {
    fe_formula = as.formula(paste0(outcome, " ~ 1 | ", unit, " + ", time))
  }
  
  mod_fe = feols(fe_formula, data = df_untreated)
  resid_sd <- sigma(mod_fe)
  
  # Get fixed effects and ensure types match
  unit_effects = data.table(
    unit = as.numeric(names(fixef(mod_fe)[[unit]])),
    unit_effect = fixef(mod_fe)[[unit]]
  )
  setnames(unit_effects, "unit", unit)
  
  time_effects = data.table(
    time = as.numeric(names(fixef(mod_fe)[[time]])),
    time_effect = fixef(mod_fe)[[time]]
  )
  setnames(time_effects, "time", time)
  
  # Merge effects back 
  df[unit_effects, unit_effect := i.unit_effect, on = eval(unit)]
  df[time_effects, time_effect := i.time_effect, on = eval(time)]
  
  # Generate predictions including control effects if present
  df[, counterfactual := get(outcome)]
  
  if(length(controls) > 0) {
    # Get coefficients for controls
    control_effects = coef(mod_fe)[controls]
    names(control_effects) = controls
    control_effects[is.na(control_effects)] = 0
    
    # Calculate total control effect
    df[treated==TRUE, 
       control_effect := Reduce(`+`, Map(function(x,y) get(x) * y, 
                                         controls, control_effects))]
    
    df[treated==TRUE, counterfactual := unit_effect + time_effect + control_effect  + 
         rnorm(.N, mean=0, sd=resid_sd)]
    df[, control_effect := NULL]
  } else {
    df[treated==TRUE, counterfactual := unit_effect + time_effect + 
         rnorm(.N, mean=0, sd=resid_sd)]
  }
  
  return(df)
}




enforce_PTA_CS = function(df, unit, group, time, rel_pass_var, outcome, controls) {
  # Get key parameters
  groups = sort(unique(df[[group]]))
  max_year = max(df[[time]])
  
  # Make a copy to store counterfactuals
  df_new = copy(df)
  df_new[, counterfactual := get(outcome)]
  
  # Iterate over each group
  for(g in groups) {
    # Get the max relative time for this group
    max_rel_pass = max(df[get(group)==g][[rel_pass_var]])
    
    # For each relative period after treatment
    for(rp in 0:max_rel_pass) {
      
      curr_time = g + rp
      if(curr_time <= max_year) {
        pre_time = g - 1
        
        # Get control group data - those not yet treated at curr_time
        control_data = df[get(group) > curr_time | is.na(get(group))]
        
        # Get pre/post periods for controls
        control_pre = control_data[get(time) == pre_time]
        control_post = control_data[get(time) == curr_time]
        
        
        # Merge pre/post for controls to calculate changes
        control_changes = merge(control_pre, control_post, 
                                by=c(unit),
                                suffixes=c("_pre", "_post"))
        
        control_changes[, delta_y := get(paste0(outcome, "_post")) - 
                          get(paste0(outcome, "_pre"))]
        
        # Use pre-period controls for regression
        if(length(controls) > 0) {
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
          treated_pre = df[get(group) == g & get(time) == pre_time]
          
          # Use pre-period controls for treated units
          if(length(controls) > 0) {
            X_treated = as.matrix(cbind(1, treated_pre[, ..controls]))
          } else {
            X_treated = as.matrix(rep(1, nrow(treated_pre)))
          }
          
          # Predict counterfactual changes
          predicted_changes = as.vector(X_treated %*% reg_model$coefficients)
          
          # Calculate counterfactual outcomes
          counterfactuals = treated_pre[[outcome]] + predicted_changes + 
            rnorm(length(predicted_changes), mean=0, sd=resid_sd)
        
          
          # Update counterfactual values
          df_new[get(group) == g & get(time) == curr_time, 
                 counterfactual := counterfactuals]
        }

      }
    }
  }
  
  return(df_new)
}
