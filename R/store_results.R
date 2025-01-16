# R/store_results.R
#' Store Power Analysis Results
#' 
#' @param results Analysis results
#' @param data Original dataset
#' @param aggregate_results Aggregated results
#' @param event_study_results Event study results
#' @param pta_type Pta type
#' @param enforce_type Enforce type
#' @param model Model type
#' @param analysis_level Analysis level
#' @param outcome Outcome variable
#' @param use_controls Control usage
#' @param drop_add_states State dropping
#' @param result_type Result type
#' @export
store_results <- function(results,
                          data,
                          aggregate_results,
                          event_study_results,
                          pta_type,
                          enforce_type,
                          model,
                          analysis_level,
                          outcome,
                          use_controls,
                          drop_add_states,
                          result_type) {
  for(model in names(results)) {
    if(model%in%c('etwfe', 'imputation')){
      att = results[[model]]$agg$estimate
      se =  results[[model]]$agg$std.error
      att_ev = results[[model]]$ev$estimate
      se_ev = results[[model]]$ev$std.error
      if(model=='etwfe'){
        rel_pass = results[[model]]$ev$event  
      } else{
        rel_pass = results[[model]]$ev$term
      }
    }
    
    
    if(model=='cs'){
      att = results[[model]]$agg$overall.att
      se =  results[[model]]$agg$overall.se
      att_ev = results[[model]]$ev$att.egt
      se_ev = results[[model]]$ev$se.egt
      rel_pass = results[[model]]$ev$egt
    }
    
    
    if(model%in%c('sa', 'sa2', 'twfe')){
      att = results[[model]]$agg$coeftable[1,1]
      se =  results[[model]]$agg$coeftable[1,2]
      att_ev = results[[model]]$ev$coeftable[,1]
      se_ev = results[[model]]$ev$coeftable[,2]
      rel_pass = rownames(results[[model]]$ev$coeftable)
    }
    
    
    aggregate_results[[length(aggregate_results) + 1]] = data.table(
      pta_type = pta_type,
      enforce_type = enforce_type,
      model = model,
      level = analysis_level,
      outcome = outcome,
      att = att,
      se = se,
      ng = length(unique(data$year_passed)), 
      n = nrow(data),
      controls = use_controls,
      ybar = mean(data[law_pass==0][[outcome]], na.rm=T),
      drop_add_states = drop_add_states,
      result_type = result_type
    )
    
    event_study_results[[length(event_study_results) + 1]] = data.table(
      pta_type = pta_type,
      enforce_type = enforce_type,
      model = model,
      level = analysis_level,
      outcome = outcome,
      rel_pass = rel_pass,
      att = att_ev,
      se = se_ev,
      controls = use_controls,
      drop_add_states = drop_add_states,
      result_type = result_type
    )
    
  }
  return(list(aggregate_results=aggregate_results,
              event_study_results=event_study_results))
  
}