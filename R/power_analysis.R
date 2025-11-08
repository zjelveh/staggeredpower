# R/power_analysis.R
#' Run Power Analysis with Parallel Trends
#' 
#' @param data_clean Clean panel dataset
#' @param unit_var Level of analysis (state or county)
#' @param pta_type Which pta assumption ("cs" or "imputation") 
#' @param enforce_type Method for enforcing parallel trends
#' @param outcome Outcome variable
#' @param controls list of controls
#' @param percent_effect Effect size to simulate
#' @param n_sims Number of simulations
#' @export
run_power_analysis <- function(data_clean,
                               unit_var,
                               group_var,
                               time_var,
                               rel_pass_var,
                               treat_ind_var,
                               controls=NULL,
                               outcome,
                               transform_outcome=NULL,
                               pta_type,
                               enforce_type=NULL,
                               percent_effect,
                               models_to_run=c('cs', 'imputation', 'twfe'),
                               n_sims = 100) {

  units_to_drop = c()
  rerun_with_dropped_units = FALSE

  nrow_full = nrow(data_clean)
  
  continue = FALSE
  data_clean_full = data_clean[between(year, 1995, 2019)]
  data_clean_copy = copy(data_clean_full)
  
  data_clean_copy[, uq_row:=paste(get(unit_var), get(time_var))]
  uq_rows = copy(data_clean_copy$uq_row)
  row_numbers = 1:nrow(data_clean_copy)
  pta_enforced_orig = enforce_PTA(
    data_clean_copy,
    unit = unit_var,
    group = group_var,
    time = time_var,
    outcome = outcome,
    controls = controls,
    method = toupper(pta_type))

    
  if(is.null(transform_outcome)){
      pta_enforced_orig[, bound_error:= ifelse(counterfactual<0, 1, 0)]
      pta_enforced_orig[bound_error==1, counterfactual:=0]
      pta_enforced_orig[, na_error:=ifelse(is.na(counterfactual), 1, 0)]

      while(!continue){
        # PTA Violation Check (run once before the simulation loop)
        pta_enforced = enforce_PTA(
          data_clean_copy,
          unit = unit_var,
          group = group_var,
          time = time_var,
          outcome = outcome,
          controls = controls,
          method = toupper(pta_type))
        
        pta_enforced[, bound_error:= ifelse(counterfactual<0, 1, 0)]
        pta_enforced[bound_error==1, counterfactual:=0]
        pta_enforced[, na_error:=ifelse(is.na(counterfactual), 1, 0)]
        

        pta_violations = copy(pta_enforced[bound_error == 1 | (na_error == 1 & get(time_var) < 2019)])
        
        if(nrow(pta_violations)==0){
          continue=TRUE
        } else{
          data_clean_copy = data_clean_copy[!get(unit_var)%in%pta_violations[[unit_var]]]
          
          units_to_drop = sort(c(units_to_drop, unique(pta_violations[[unit_var]])))
        }
        if(nrow(data_clean_copy)==0){
          continue=TRUE
        }
      }
      
    
  }

  share_rows_dropped = 1 - (nrow(data_clean_copy) / nrow(data_clean_full))
  share_units_dropped = 1 - (length(unique(data_clean_copy[[unit_var]])) / 
                               length(unique(data_clean_full[[unit_var]])))
  share_groups_dropped = 1 - (length(unique(data_clean_copy[[group_var]])) / 
                               length(unique(data_clean_full[[group_var]])))
  
  # N treated groups
  n_treated_groups = length(unique(data_clean_copy[get(treat_ind_var)==1][[group_var]]))
  
  if(n_treated_groups >= 4 & is.null(transform_outcome)){
    rerun_with_dropped_units = TRUE  # Mark for rerun with dropped groups
  }


  pta_enforced_orig[, dropped:=ifelse(uq_row%in%data_clean_copy$uq_row, 0, 1)]
  
  cat("Num rows original", nrow(data_clean_full), '\n')
  cat("Num rows final", nrow(data_clean_copy), '\n')
  cat('number of pta violations orig', sum(pta_enforced_orig$bound_error), '\n')
  cat('number of pta violations after', sum(pta_enforced_orig$dropped), '\n')
  cat('number of pta violations while', nrow(data_clean_full) - nrow(data_clean_copy), '\n')
  

  final_power_list = list()
  final_vio_list = list()

  
  # Loop for two passes: first without dropping groups, then with (if violations exist)
  for (run_iteration in 1:1){#:(ifelse(rerun_with_dropped_units, 2, 1))) {
    dat_clean = if (run_iteration == 1) data_clean_full else data_clean_copy
    dat_clean = dat_clean[!is.na(get(outcome))]
    
    rez_list = 
      foreach(sim = 1:n_sims,
              .packages = c('data.table', 'fixest', 'did2s', 
                            'did', 'didimputation')) %dopar% 
      {
        new_temp = list()
        
        # Generate counterfactuals for each simulation
        pta_enforced_sim = enforce_PTA(
          dat_clean,
          unit = unit_var,
          group = group_var,
          time = time_var,
          outcome = outcome,
          controls = controls,
          method = toupper(pta_type))
        # Now scale the counterfactual outcomes within the simulation
        counterfactual_data = copy(pta_enforced_sim)
        counterfactual_data[, y_cf := counterfactual]
        if(!is.null(transform_outcome)){
          if(transform_outcome=='log'){
            counterfactual_data[get(treat_ind_var) == 1, y_cf := y_cf + log(percent_effect)]
          }
        } else{
          counterfactual_data[get(treat_ind_var) == 1, y_cf := y_cf * percent_effect]
        }
        
        model_data = copy(counterfactual_data)

        if(sum(model_data[[treat_ind_var]])>0){
          # Model estimation
          results = estimate_models(
            data = model_data,
            id_var = unit_var,
            outcome_var = 'y_cf',
            time_var = time_var,
            group_var = group_var,
            controls = controls,
            treat_ind_var = treat_ind_var,
            models_to_run = models_to_run)
          # drop groups
          if(is.null(transform_outcome)){
            drop_groups = unique(pta_violations[[group_var]])
            
          } else{
            drop_groups = c()
          }

          for(model in names(results)){
            if(model=='imputation'){
              att = results[[model]]$agg$estimate
              se =  results[[model]]$agg$std.error
            }
            
            if(model=='cs'){
              att = results[[model]]$agg$overall.att
              se =  results[[model]]$agg$overall.se
            }
            
            if(model=='cs_reg'){
              att = results[[model]]$agg$overall.att
              se =  results[[model]]$agg$overall.se
            }
            
            
            if(model%in%c('sa', 'twfe')){
              att = results[[model]]$agg$coeftable[1, 1]
              se =  results[[model]]$agg$coeftable[1, 2]
            }

            new_temp[[length(new_temp) + 1]] = data.table(
              model = model,
              level = unit_var,
              outcome = outcome,
              percent_effect = percent_effect,
              pta_type =pta_type,
              enforce_type = ifelse(is.null(enforce_type), 'no_controls', paste0(enforce_type, collapse='*')),
              controls = ifelse(is.null(controls), 'no_controls', paste0(controls, collapse='*')),
              att = att,
              se = se,
              dropped_unit_list = paste0(units_to_drop, collapse=' '),
              y0_bar_csa = mean(data.table(results$cs$agg$DIDparams$data)[get(group_var)>get(time_var)]$y_cf, na.rm=T),
              y0_bar = mean(model_data[!is.na(y_cf)][get(treat_ind_var)==0][[outcome]], na.rm=T),
              sim = sim,
              iteration = run_iteration,
              n_dropped_units = length(units_to_drop),
              share_rows_dropped = share_rows_dropped,
              share_units_dropped = share_units_dropped,
              share_groups_dropped = share_groups_dropped,
              outcome_transformed = ifelse(is.null(transform_outcome), 'No', 'log')
            )
          }
          
          returnz = list(results=rbindlist(new_temp))
          
        } 
          
        
        return(returnz) 
      }
    

    # Inside the run_iteration loop, after foreach loop
    for(i in rez_list) {
      final_power_list[[length(final_power_list)+1]] = i$results
    }
    
    if(is.null(transform_outcome)){
      final_vio = copy(pta_violations)
      final_vio[, iteration:=run_iteration]
      final_vio[, level:=unit_var]
      final_vio[, n:=nrow(dat_clean)]
      final_vio[, controls:= ifelse(is.null(controls), 'no_controls', paste0(controls, collapse='*'))]
      final_vio[, enforce_type:= ifelse(is.null(enforce_type), 'no_controls', paste0(enforce_type, collapse='*'))]
      final_vio = final_vio[, .(iteration, counterfactual, bound_error,
                                na_error, level,
                                unit=get(unit_var), 
                                group=get(group_var),
                                time=get(time_var), 
                                outcome,
                                pta_type, 
                                controls, 
                                enforce_type)
      ]
      final_vio_list[[length(final_vio_list) + 1]] = final_vio
    }

  }

  final_power = rbindlist(final_power_list)
  
  if(is.null(transform_outcome)){
    final_vio = rbindlist(final_vio_list)
  } else{
    final_vio = NA
  }
 return(list(final_vio=final_vio, 
             final_power=final_power))
}
