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
                        enforce_type = 'simple') {
  
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
#' @param enforce_type Character string specifying whether to include controls ("controls") or 
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
enforce_PTA_imputation = function(df, unit, group, time, outcome, enforce_type){
  # Split into pre/post treatment periods  
  df[, treated := get(time) > get(group)]
  df_untreated = df[treated==FALSE]
  
  # Estimate FEs
  if(enforce_type=='controls'){
    fe_formula = as.formula(paste0(outcome, '~ unemp_rate | ', unit, ' + ', time))
  } else {
    fe_formula = as.formula(paste0(outcome, '~ 1 | ', unit, ' + ', time))
  }
  
  mod_fe = feols(fe_formula, data = df_untreated)
  
  # Get fixed effects and ensure types match
  unit_effects = data.table(
    unit = as.numeric(names(fixef(mod_fe)[[unit]])), # or as.character() depending on your data
    unit_effect = fixef(mod_fe)[[unit]]
  )
  setnames(unit_effects, "unit", unit) # explicitly set column name to match
  
  time_effects = data.table(
    time = as.numeric(names(fixef(mod_fe)[[time]])), # or as.character()
    time_effect = fixef(mod_fe)[[time]]
  )
  setnames(time_effects, "time", time) # explicitly set column name to match
  
  # Merge effects back 
  df[unit_effects, unit_effect := i.unit_effect, on = eval(unit)]
  df[time_effects, time_effect := i.time_effect, on = eval(time)]
  
  # Generate predictions 
  df[, counterfactual := get(outcome)]
  
  if(enforce_type == 'controls') {
    controls_effect = df[treated==TRUE, unemp_rate] * coef(mod_fe)['unemp_rate']
    df[treated==TRUE, counterfactual := unit_effect + time_effect + controls_effect]
  } else {
    df[treated==TRUE, counterfactual := unit_effect + time_effect]
  }
  
  return(df)
}

#' Enforce parallel trends using Callaway & Sant'Anna approach
#'
#' @description
#' Implements the Callaway & Sant'Anna (2021) approach to enforcing parallel trends
#' in staggered difference-in-differences designs. For each treated group and time period,
#' uses not-yet-treated units as controls to compute counterfactual outcomes.
#'
#' @param df A data.table containing the panel data
#' @param unit Column name for unit identifiers 
#' @param group Column name for treatment cohort/group
#' @param time Column name for time periods
#' @param rel_pass_var Column name for relative time since treatment
#' @param outcome Column name for outcome variable
#' @param enforce_type Character string specifying sampling approach:
#'   - "simple": Independent lognormal sampling around target mean
#'   - "simple_correlated": Lognormal sampling preserving pre-treatment rank order
#'
#' @return A data.table in long format with counterfactual outcomes for treated observations
#'
#' @details
#' For each treatment group g and relative time period rp:
#' 1. Identifies control units not yet treated by time g + rp
#' 2. Calculates mean outcome change for controls between pre/post periods
#' 3. Uses this to compute target mean outcome for treated units
#' 4. Generates counterfactual outcomes via sampling to achieve target mean
#'
#' Sampling methods:
#' - simple: Independent lognormal draws around target mean
#' - simple_correlated: Preserves rank ordering from pre-treatment period
#'
#' Requires balanced panel without missing data.
#'
#' @references
#' Callaway, B., & Sant'Anna, P. H. C. (2021). Difference-in-differences with multiple time periods. 
#' Journal of Econometrics, 225(2), 200-230.
#'
#' @examples
#' df <- data.table(
#'   unit = rep(1:10, each=5),
#'   time = rep(2000:2004, 10), 
#'   group = rep(c(2001,2002,2003), length.out=10),
#'   y = rnorm(50),
#'   rel_time = rep(-2:2, 10)
#' )
#' result <- enforce_PTA_CS(df, "unit", "group", "time", "rel_time", "y", "simple")
#' @export
enforce_PTA_CS = function(df, unit, group, time, rel_pass_var, 
                          outcome, enforce_type){
  
  # Create a wide format data frame
  df_wide = create_wide(df, 
                        group,
                        unit,
                        time,
                        outcome)
  
  
  min_year = as.numeric(gsub('yr_', '', names(df_wide)[3]))
  
  max_year = max(df[[group]])
  
  # Get the sorted unique group values
  groups = sort(unique(df[[group]]))
  
  # Iterate over each group
  for(g in groups){
    pre_year_col = paste0('yr_', g - 1) # year before treatment
    pre_years_col = paste0('yr_', min_year : (g-1))
    
    # Get the unique units for the current group
    units = unique(df[get(group)==g][[unit]])
    
    # Get the maximum relative time since treatment for the current group
    max_rel_pass = max(df[get(group)==g][[rel_pass_var]])
    
    # Iterate over each relative time period
    for(rp in 0 : max_rel_pass){
      if(g + rp <= max_year - 1 & g <= max_year){
        
        # Enforce the specified parallel trends assumption
        diffs=NA
        
        # Define column names for the current post-treatment year, pre-treatment year, and all pre-treatment years
        this_year_col = paste0('yr_', g + rp) # current post treatment year
        
        # Identify control groups (not yet treated by the current post-treatment year)
        # Subset the data for the control groups
        y0_C = df_wide[get(group) > g + rp,
                       c(pre_year_col, this_year_col, group),
                       with=FALSE]
        
        # Calculate the average difference between the current post-treatment year 
        # and the pre-treatment year for the control groups
        diffs = mean(y0_C[[this_year_col]] - y0_C[[pre_year_col]], na.rm=TRUE) 

        y0_T = df_wide[get(group)==g,
                       c(pre_year_col, this_year_col, unit, group),
                       with=FALSE]
        
        target_ybar_post = diffs + mean(y0_T[[pre_year_col]], na.rm=T)
        
        if(target_ybar_post < 0) {
          samples <- rep(target_ybar_post, nrow(y0_T))
        } else {
          # Branch based on enforcement type
          if(enforce_type == 'simple') {
            # Simple version: Just use lognormal sampling
            cv <- sd(y0_C[[pre_year_col]])/mean(y0_C[[pre_year_col]])
            samples <- sample_lognormal_with_mean(nrow(y0_T), 
                                                  target_ybar_post, 
                                                  cv)
          } 
          
          if(enforce_type=='simple_correlated'){
            pre_means <- y0_T[, .(pre_mean = mean(rowMeans(.SD, na.rm=TRUE))), 
                              by=unit,
                              .SDcols = names(y0_T)[names(y0_T) %in% pre_years_col]]
            
            # Get rank order of pre-period means
            pre_means[, rank := rank(pre_mean, ties.method="random")]
            
            
            # Generate lognormal samples
            cv <- sd(y0_C[[pre_year_col]])/mean(y0_C[[pre_year_col]])
            raw_samples <- sample_lognormal_with_mean(nrow(y0_T), 
                                                      target_ybar_post, 
                                                      cv)
            
            # Order the samples from smallest to largest
            ordered_samples <- sort(raw_samples)
            
            # Create mapping of ranks to samples
            samples_by_rank <- data.table(
              rank = 1:length(ordered_samples),
              sample = ordered_samples
            )
            
            # Merge ranks with samples
            pre_means <- merge(pre_means, samples_by_rank, by="rank")
            
            # Create final samples vector in original unit order
            samples <- merge(y0_T[, .(unit=get(unit))], 
                             pre_means[, .(unit=get(unit), sample)], 
                             by="unit")$sample
          }
        } 
        
        
        # Assign samples back to the data for this group and this rp (same as before)
        y0_T[, replace_value := samples]
        
        df_wide = merge(df_wide,
                        y0_T[, c(unit, 'replace_value'),
                             with=F],
                        all.x=T,
                        by=unit)
        
        df_wide[!is.na(replace_value), 
                eval(this_year_col):=replace_value]
        
        df_wide[, replace_value:=NULL]
        
      } 
    }
  }
  
  # Convert the wide format data frame back to long format
  df_new = melt(df_wide,
                id.vars = c(unit, group),
                value.name = 'counterfactual',
                variable.name=time)
  
  # Convert the time variable to numeric
  df_new[, eval(time):=as.numeric(gsub('yr_', '', get(time)))]
  
  
  
  df_new = merge(df, df_new, by=c(unit, group, time))
  
  return(df_new)
}

