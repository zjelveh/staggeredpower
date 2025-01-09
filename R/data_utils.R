# R/data_utils.R
#' Create Wide Format Dataset
#' 
#' @param df Long format dataset
#' @param group Group variable
#' @param unit Unit identifier 
#' @param time Time variable
#' @param outcome Outcome variable
#' @export
create_wide <- function(df,
                        group,
                        unit, 
                        time,
                        outcome) {
  # Reshape the data frame from long to wide format
  df = df[!is.na(get(outcome))]
  y0_wide = dcast(df, 
                  get(unit)+get(group) ~ get(time), 
                  value.var=outcome)
  
  # Rename the time columns to have a "yr_" prefix
  setnames(y0_wide, 
           3:ncol(y0_wide),  
           paste0('yr_', names(y0_wide)[3:ncol(y0_wide)]))
  
  # Rename the unit and group columns
  setnames(y0_wide, 1:2, c(unit, group))
  y0_wide = y0_wide[, colMeans(is.na(y0_wide))<1, with=FALSE]
  
  y0_wide = y0_wide[ rowMeans(is.na(y0_wide[, 3:ncol(y0_wide)]))==0]
  
  
  return(y0_wide)
}



#' Sample from Lognormal Distribution with Target Mean
#' 
#' @param n Number of samples
#' @param desired_mean Target mean
#' @param cv Coefficient of variation
#' @export
sample_lognormal_with_mean <- function(n, desired_mean, cv = 0.5) {
  # Calculate sigma
  sigma <- sqrt(log(1 + cv^2))
  
  # Calculate mu
  mu <- log(desired_mean) - sigma^2 / 2
  
  # Generate samples
  samples <- rlnorm(n, meanlog = mu, sdlog = sigma)
  
  return(samples)
  
}