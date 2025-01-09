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
                        max_year = NULL,
                        pta_type = 'cs',
                        enforce_type = 'simple',
                        rho_hat,
                        sd_resid, 
                        time_fe,
                        unit_fe) {
  # Create a wide format data frame
  df_wide = create_wide(df, 
                        group,
                        unit,
                        time,
                        outcome)
  
  # Get the sorted unique group values
  groups = sort(unique(df[[group]]))
  
  stats = list()
  
  # Iterate over each group
  for(g in groups){
    # Get the unique units for the current group
    units = unique(df[get(group)==g][[unit]])
    
    # Get the maximum relative time since treatment for the current group
    max_rel_pass = max(df[get(group)==g][[rel_pass_var]])
    if(is.null(max_year))
      max_year = max(df[[group]])
    
    # Iterate over each relative time period
    for(rp in 0 : max_rel_pass){
      if(g + rp <= max_year & g <= max_year){
        # Enforce the specified parallel trends assumption
        df_wide2 = PTA_type(
          df_wide, 
          unit, 
          group,
          time, 
          g, 
          rp,
          pta_type,
          enforce_type,
          rho_hat,
          sd_resid,
          time_fe,
          unit_fe,
          max_year,
          min_year=as.numeric(gsub('yr_', '', names(df_wide)[3]))
        )  
        df_wide = df_wide2$df_wide
        stats[[length(stats) + 1]] = data.table(
          group=df_wide2$group,  
          time=df_wide2$time,
          na_error=df_wide2$na_error,
          bound_error=df_wide2$bound_error,
          target_ybar_post=df_wide2$target_ybar_post
        )
      }
      
    }
    
    # Convert the wide format data frame back to long format
    df_new = melt(df_wide,
                  id.vars = c(unit, group),
                  value.name = outcome,
                  variable.name=time)
    
    # Convert the time variable to numeric
    df_new[, eval(time):=as.numeric(gsub('yr_', '', get(time)))]
  }
  
  return(list(df_new=df_new, stats=stats))
}




#' Enforce parallel trends assumption (PTA)
#'
#' This function enforces different types of parallel trends assumptions on a wide format data frame.
#'
#' @param df_wide The wide format data frame
#' @param pta_type The type of parallel trends assumption to enforce. Options are 'pre_year', 'pre_years', 'max_untreated_year', and 'diffs'.
#' @param unit The column name for the unit variable
#' @param group The column name for the group variable
#' @param time The column name for the time variable
#' @param g The group being treated
#' @param rp The relative period (time since treatment)
#' @param min_year The earliest year in the data
#'
#' @return The modified wide format data frame with the parallel trends assumption enforced
#'
#' @examples
#' df_wide <- PTA_type(df_wide, "pre_year", "unit", "group", "time", 1, 0, 2000)
PTA_type <- function(df_wide,
                     unit,
                     group,
                     time,
                     g,
                     rp, 
                     pta_type,
                     enforce_type,
                     rho_hat,        # AR(1) coefficient estimated from data
                     sd_resid,       # Residual standard deviation
                     time_fe,        # Time fixed effects
                     unit_fe,        # Unit fixed effects
                     max_year,
                     min_year) {
  
  
  period='pre_year'
  diffs=NA
  bound_error=FALSE
  na_error=FALSE
  
  # Define column names for the current post-treatment year, pre-treatment year, and all pre-treatment years
  this_year_col = paste0('yr_', g + rp) # current post treatment year
  pre_year_col = paste0('yr_', g - 1) # year before treatment
  pre_years_col = paste0('yr_', min_year : (g-1))
  
  if(pta_type=='cs'){
    # Identify control groups (not yet treated by the current post-treatment year)
    # Subset the data for the control groups
    y0_C = df_wide[get(group) > g + rp,
                   c(pre_year_col, this_year_col, group),
                   with=FALSE]
    
    if(nrow(y0_C)>0){
      # Calculate the average difference between the current post-treatment year 
      # and the pre-treatment year for the control groups
      diffs = mean(y0_C[[this_year_col]] - y0_C[[pre_year_col]], na.rm=TRUE) 
    }
    
    y0_T = df_wide[get(group)==g,
                   c(pre_year_col, this_year_col, unit, group),
                   with=FALSE]
    
    target_ybar_post = diffs + mean(y0_T[[pre_year_col]], na.rm=T)
    
    
    if(!is.na(target_ybar_post)) {
      if(target_ybar_post < 0) {
        bound_error = TRUE
        samples <- rep(0, nrow(y0_T))
      } else {
        # Branch based on enforcement type
        if(enforce_type == 'simple') {
          # Simple version: Just use lognormal sampling
          cv <- sd(y0_C[[pre_year_col]])/mean(y0_C[[pre_year_col]])
          samples <- sample_lognormal_with_mean(nrow(y0_T), 
                                                target_ybar_post, 
                                                cv)
        } 
        if(enforce_type == 'realistic') {
          # Realistic version with AR(1) process
          samples <- numeric(nrow(y0_T))
          
          for(i in seq_len(nrow(y0_T))) {
            unit_i <- y0_T[i, get(unit)]
            
            # Get unit and time fixed effects
            unit_effect <- unit_fe[unit == as.character(unit_i), unit_effect]
            time_effect <- time_fe[time == as.character(g + rp), time_effect]
            
            # Get last period's actual outcome for AR(1)
            last_outcome <- df_wide[get(unit) == unit_i, get(pre_year_col)]
            last_predicted <- unit_effect + 
              time_fe[time == as.character(g-1), time_effect]
            last_resid <- last_outcome - last_predicted
            
            # Generate new residual using AR(1)
            new_resid <- rho_hat * last_resid + 
              rnorm(1, 0, sd_resid * sqrt(1 - rho_hat^2))
            
            # Combine components
            samples[i] <- unit_effect + time_effect + new_resid
          }
          
          # Rescale to preserve parallel trends
          scaling_factor <- target_ybar_post / mean(samples)
          samples <- samples * scaling_factor
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
      
      
      # Assign samples back to the data (same as before)
      y0_T[, replace_value := samples]
      
      df_wide = merge(df_wide,
                      y0_T[, c(unit, 'replace_value'),
                           with=F],
                      all.x=T,
                      by=unit)
      
      df_wide[!is.na(replace_value), 
              eval(this_year_col):=replace_value]
      
    } else {
      na_error = TRUE
      y0_T[, replace_value := NA]
      
      df_wide = merge(df_wide,
                      y0_T[, c(unit, 'replace_value'),
                           with=F],
                      all.x=T,
                      by=unit)
      
      df_wide[, eval(this_year_col):=NA]
    }
    
    df_wide[, replace_value:=NULL]
    
  }
  
  
  if(pta_type=='imputation'){
    # Get treated units for current period
    y0_T = df_wide[get(group)==g,
                   c(unit, this_year_col),
                   with=FALSE]
    
    # For each treated unit
    samples = numeric(nrow(y0_T))
    for(i in seq_len(nrow(y0_T))) {
      unit_i <- y0_T[i, get(unit)]
      
      # Get point prediction
      mean_pred <- unit_fe[unit == as.character(unit_i), unit_effect] + 
        time_fe[time == as.character(g + rp), time_effect]
      
      # Generate random counterfactual
      
      samples[i] <- rnorm(1, mean_pred, sd_resid)
      if(samples[i]<0){
        bound_error=TRUE
      }
      
    }
    
    # Replace values in wide data
    y0_T[, replace_value := samples]
    
    df_wide = merge(df_wide,
                    y0_T[, c(unit, 'replace_value'),
                         with=F],
                    all.x=T,
                    by=unit)
    
    df_wide[!is.na(replace_value), 
            eval(this_year_col):=replace_value]
    
    df_wide[, replace_value:=NULL]
    
    target_ybar_post = mean(samples, na.rm=TRUE)
    
  }
  
  
  return(list(df_wide=df_wide,
              group=g,
              time=g+rp,
              bound_error=bound_error,
              na_error=na_error,
              target_ybar_post=target_ybar_post))
}

