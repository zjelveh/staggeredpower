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
                               treat_ind,
                               pta_type,
                               enforce_type,
                               outcome,
                               controls=NULL,
                               percent_effect,
                               models_to_run=c('cs', 'imputation', 'twfe'),
                               n_sims = 100,
                               max_year=2019) {
  

  generate_key <- function(analysis_level, pta_type, enforce_type, outcome, controls, percent_effect, run_iteration) {
    enforce_type = ifelse(length(enforce_type)==0, 'no_controls', paste0(enforce_type, collapse='*'))
    controls = ifelse(length(controls)==0, 'no_controls', paste0(controls, collapse='*'))
    return(paste(analysis_level, pta_type, enforce_type, outcome, controls, percent_effect, run_iteration, sep='__', collapse = "__"))
  }
  
  
  groups_to_drop = c()
  rerun_with_dropped_groups = FALSE
  
  # PTA Violation Check (run once before the simulation loop)
  pta_enforced = enforce_PTA(
    data_clean[between(year, 1995, 2019)],
    unit = unit_var,
    group = group_var,
    time = time_var,
    rel_pass_var = rel_pass_var,
    outcome = outcome,
    pta_type = pta_type,
    enforce_type = enforce_type)

  pta_violations = copy(pta_enforced[bound_error == 1 | (na_error == 1 & get(time_var) < 2019)])

  if (nrow(pta_violations) > 0) {
    groups_to_drop = unique(pta_violations[[group_var]])
    rerun_with_dropped_groups = TRUE  # Mark for rerun with dropped groups
    cat("PTA violations detected. Groups to drop:", paste(sort(groups_to_drop), collapse = ", "), "\n")
  }
  
  final_power_list = list()
  final_vio_list = list()
  
  # Loop for two passes: first without dropping groups, then with (if violations exist)
  for (run_iteration in 1:(ifelse(rerun_with_dropped_groups, 2, 1))) {
    key = generate_key(unit_var, pta_type, enforce_type, outcome,
                       controls, percent_effect, run_iteration)
    
    # Only run if groups_to_drop is less than 50% of all groups
    if(rerun_with_dropped_groups){
      share_dropped = length(groups_to_drop)/length(unique(data_clean[[group_var]]))
    } else{
      share_dropped = 0
    }

    if(share_dropped < .5 | run_iteration==1){
      dat_clean = if (run_iteration == 1) data_clean else data_clean[!get(group_var) %in% groups_to_drop]
      
      rez_list = 
        foreach(sim = 1:n_sims,
                .packages = c('data.table', 'fixest', 'did2s', 
                              'did', 'didimputation')) %dopar% 
        {
          new_temp = list()
          
          # Generate counterfactuals for each simulation
          pta_enforced_sim = enforce_PTA(
            dat_clean[between(year, 1995, 2019)],
            unit = unit_var,
            group = group_var,
            time = time_var,
            rel_pass_var = rel_pass_var,
            outcome = outcome,
            pta_type = pta_type,
            enforce_type = enforce_type)

          temp = pta_enforced_sim[!is.na(counterfactual)]
          
          # Now scale the counterfactual outcomes within the simulation
          counterfactual_data = copy(pta_enforced_sim)
          counterfactual_data[, y_cf := counterfactual]
          counterfactual_data[get(treat_ind) == 1, y_cf := y_cf * percent_effect, by = group_var]
          
          if (grepl('aggshare', outcome)) {
            counterfactual_data[, y_cf := ifelse(y_cf > 1, 1, y_cf)]
          }
          
          model_data = copy(counterfactual_data)
          
          
          if(sum(model_data[[treat_ind]])>0){
            # Further steps for computing empirical treatment effects and saving results...
            te_computed = compute_te(df = model_data,
                                     pta_type=pta_type,
                                     enforce_type=enforce_type,
                                     outcome = 'y_cf',
                                     group_var = group_var,
                                     time_var = time_var,
                                     rel_pass_var = rel_pass_var)
            
            te_computed = rbindlist(te_computed)
            te_computed = te_computed[!is.na(te)]
            
            # Model estimation
            results = estimate_models(
              data = model_data,
              id_var = unit_var,
              outcome_var = 'y_cf',
              time_var = time_var,
              group_var = group_var,
              arrest_law_type = 'all',
              controls = controls,
              models_to_run = models_to_run)
            
            # get weights
            did_weights = data.table(results$cs$agg$DIDparams$data)
            did_weights = did_weights[get(time_var)==min(get(time_var))]
            did_weights = did_weights[, .(group_share=.N / nrow(did_weights)), by=group_var]
            te_computed = merge(te_computed, did_weights, by.x='group', by.y=group_var)
            empirical_te = sum(te_computed$te * te_computed$group_share) / sum(te_computed$group_share)
            te_computed[, group:=as.numeric(group)]
            te_computed[, time:=as.numeric(time)]

            # drop groups
            drop_groups = unique(pta_violations[[group_var]])
            
            for(model in names(results)){
              if(model=='imputation'){
                att = results[[model]]$agg$estimate
                se =  results[[model]]$agg$std.error
              }
              
              if(model=='cs'){
                att = results[[model]]$agg$overall.att
                se =  results[[model]]$agg$overall.se
              }
              
              
              if(model%in%c('sa', 'twfe')){
                att = results[[model]]$agg$coeftable[1, 1]
                se =  results[[model]]$agg$coeftable[1, 2]
              }
              

             new_temp[[length(new_temp) + 1]] = data.table(
                model=model,
                level=unit_var,
                outcome = outcome,
                percent_effect = percent_effect,
                pta_type=pta_type,
                enforce_type=ifelse(is.null(enforce_type), 'simple', paste0(enforce_type, collapse='*')),
                controls = ifelse(is.null(controls), 'no controls', paste0(controls, collapse='*')),
                att = att,
                se = se,
                group_list_csa = paste0(results[['cs']]$agg$DIDparams$glist, collapse=' '),
                ng_csa = results[['cs']]$agg$DIDparams$nG,
                ng = length(unique(model_data[[group_var]])),
                n = results[['cs']]$agg$DIDparams$n,
                nT = results[['cs']]$agg$DIDparams$nT,
                ss_csa = nrow(results$cs$agg$DIDparams$data),
                ss = nrow(model_data[!is.na(y_cf)]),
                y0_bar_csa = mean(data.table(results$cs$agg$DIDparams$data)[get(group_var)>get(time_var)]$y_cf, na.rm=T),
                y0_bar = mean(model_data[!is.na(y_cf)][get(treat_ind)==0][[outcome]], na.rm=T),
                sim = sim,
                empirical_te = empirical_te,
                iteration=run_iteration,
                n_dropped_groups = length(groups_to_drop),
                unique_key = key
              )
            }
            
            
            returnz = list(results=rbindlist(new_temp))
          } else{
            print('going in here')
            new_temp[[length(new_temp) + 1]] = data.table(
              model=NA,
              level=unit_var,
              outcome = outcome,
              percent_effect = percent_effect,
              pta_type=pta_type,
              enforce_type=ifelse(is.null(enforce_type), 'simple', paste0(enforce_type, collapse='*')),
              controls = ifelse(is.null(controls), 'no controls', paste0(controls, collapse='*')),
              att = NA,
              se = NA,
              group_list_csa = NA,
              ng_csa = NA,
              ng = length(unique(model_data[[group_var]])),
              n = NA,
              nT = NA,
              ss_csa = NA,
              ss = nrow(model_data[!is.na(y_cf)]),
              y0_bar_csa = NA,
              y0_bar = mean(model_data[!is.na(y_cf)][get(treat_ind)==0][[outcome]], na.rm=T),
              sim = sim,
              empirical_te = NA,
              iteration=run_iteration,
              n_dropped_groups = length(groups_to_drop),
              unique_key = key
            )
            
            returnz = list(results=rbindlist(new_temp))
          }
          return(returnz) 
          
        }
      
      
      # Inside the run_iteration loop, after foreach loop
      for(i in rez_list) {
        i$results[, unique_key := key]
        final_power_list[[length(final_power_list)+1]] = i$results
      }
      final_vio = copy(pta_enforced)
      final_vio[, unique_key := key]
      final_vio[, iteration:=run_iteration]
      final_vio[, controls:= ifelse(is.null(controls), 'no_controls', paste0(controls, collapse='*'))]
      final_vio[, enforce_type:= ifelse(is.null(enforce_type), 'no_controls', paste0(enforce_type, collapse='*'))]
      final_vio = final_vio[, .(unique_key, iteration, counterfactual, bound_error,
                                na_error,
                                unit=get(unit_var), group=get(group_var), time=get(time_var), outcome,
                                pta_type, controls, enforce_type)
                                ]
      final_vio_list[[length(final_vio_list) + 1]] = final_vio
    }
    
  }
  
  
  final_power = rbindlist(final_power_list)
  
  final_vio = rbindlist(final_vio_list)
  
  return(list(final_vio=final_vio, 
              final_power=final_power))
}
