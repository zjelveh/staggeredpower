# R/compute_te.R 
#' Compute Treatment Effects
#' 
#' @param df Dataset
#' @param pta_type Which pta assumption ("cs" or "imputation") 
#' @param enforce_type Method for enforcing parallel trends
#' @param group Group variable
#' @param time_var time_var variable
#' @param rel_pass_var rel_pass_var variable
#' @param outcome Outcome variable
#' @export
compute_te <- function(df,
                       pta_type,
                       enforce_type, 
                       group_var,
                       time_var,
                       rel_pass_var,
                       outcome) {

  max_year = max(unique(df[[group_var]]))
  df = df[!is.na(get(outcome))]
  gt = list()
  groups = sort(unique(df[[group_var]]))
  for(g in groups[groups<max_year]){
    for(yr in g:max_year){
      yt_post = mean(df[get(group_var)==g & get(time_var)==yr][[outcome]])
      if(pta_type=='cs'){
        yt_pre = mean(df[get(group_var)==g & get(rel_pass_var)==-1][[outcome]])
        yc_pre = mean(df[get(group_var)>yr & get(time_var)==g-1][[outcome]])
        
        yc_post = mean(df[get(group_var)>yr & get(time_var)==yr][[outcome]])
        gt[[length(gt) + 1]] = data.table(
          group = g,
          time = yr,
          yt_post = yt_post,
          yt_pre = yt_pre,
          yc_post = yc_post,
          yc_pre = yc_pre,
          te = (yt_post-yt_pre) - (yc_post-yc_pre)
        )
      }
      if(pta_type=='imputation'){
        yt_pre = mean(df[get(group_var)==g & get(rel_pass_var)<=-1][[outcome]])
        yc_pre = mean(df[get(group_var)>yr & get(time_var)<=g-1][[outcome]])
        
        yc_post = mean(df[get(group_var)>yr & get(time_var)==yr][[outcome]])
        gt[[length(gt) + 1]] = data.table(
          group = g,
          time = yr,
          yt_post = yt_post,
          yt_pre = yt_pre,
          yc_post = yc_post,
          yc_pre = yc_pre,
          te = (yt_post-yt_pre) - (yc_post-yc_pre)
        )
      }
    }
  }
  return(gt)
}