# R/compute_te.R 
#' Compute Treatment Effects
#' 
#' @param df Dataset
#' @param period Pre-period type
#' @param cohort Control group type
#' @param group Group variable
#' @param time Time variable
#' @param max_year Maximum year
#' @param outcome Outcome variable
#' @export
compute_te <- function(df,
                       period,
                       cohort, 
                       group,
                       time,
                       max_year = NULL,
                       outcome) {

  df = df[!is.na(get(outcome))]
  gt = list()
  groups = sort(unique(df[[group]]))
  for(g in groups[groups<max_year]){
    for(yr in g:max_year){
      yt_post = mean(df[get(group)==g & get(time)==yr][[outcome]])
      if(period=='pre_year' & cohort=='notyettreated'){
        yt_pre = mean(df[get(group)==g & rel_pass==-1][[outcome]])
        yc_pre = mean(df[get(group)>yr & get(time)==g-1][[outcome]])
        
        yc_post = mean(df[get(group)>yr & get(time)==yr][[outcome]])
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
      if(period=='pre_year' & cohort=='lasttreated'){
        yt_pre = mean(df[get(group)==g & rel_pass==-1][[outcome]])
        yc_pre = mean(df[get(group)==max_year+1 & get(time)==g-1][[outcome]])
        
        yc_post = mean(df[get(group)==max_year+1 & get(time)==yr][[outcome]])
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
      if(period=='pre_years' & cohort=='lasttreated'){
        
        yt_pre = mean(df[get(group)==g & rel_pass<=-1][[outcome]])
        yc_pre = mean(df[get(group)==max_year+1 & get(time)<=g-1][[outcome]])
        
        yc_post = mean(df[get(group)==max_year+1 & get(time)==yr][[outcome]])
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
      if(period=='pre_years' & cohort=='notyettreated'){
        yt_pre = mean(df[get(group)==g & rel_pass<=-1][[outcome]])
        yc_pre = mean(df[get(group)>yr & get(time)<=g-1][[outcome]])
        
        yc_post = mean(df[get(group)>yr & get(time)==yr][[outcome]])
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