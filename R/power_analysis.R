# R/power_analysis.R
#' Run Power Analysis with Parallel Trends
#' 
#' @param data_clean Clean panel dataset
#' @param unit_var Level of analysis (state or county)
#' @param pta_type Which pta assumption ("cs" or "imputation") 
#' @param enforce_type Method for enforcing parallel trends
#' @param outcome Outcome variable
#' @param use_controls Whether to use controls
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
                               max_year=2019,
                               use_controls,
                               percent_effect,
                               models_to_run=c('cs', 'imputation', 'twfe'),
                               n_sims = 100) {
  # compute feols_model
  frm_fe = as.formula(paste0(outcome, '~1 | ', unit_var, '+',  time_var))
  
  df_pre = data_clean[get(treat_ind)==0]
  mod_fe <- feols(frm_fe, data = df_pre)
  df_pre[, resid_fe := resid(mod_fe)]
  
  # estimate AR1 model
  # Let's do something naive: compute the AR(1) for each unit and maybe average them
  ar_list <- list()
  units_pre <- unique(df_pre[[unit_var]])
  
  for(u in units_pre) {
    u_resids <- df_pre[get(unit_var) == u, resid_fe]
    if(length(u_resids) > 1) {
      # Fit AR(1). Example: stats::ar()
      # Or do a manual lag correlation approach.
      fit_ar <- ar(u_resids, order.max = 1, aic = FALSE)
      ar_list[[u]] <- fit_ar$ar
    }
  }
  
  rho_hat <- mean(unlist(ar_list), na.rm = TRUE)  # average AR(1) parameter
  sd_resid <- sd(df_pre$resid_fe, na.rm = TRUE)   # overall stdev of residual
  
  unit_fixed_effects = data.table(
    unit = names(fixef(mod_fe)[[unit_var]]),
    unit_effect = fixef(mod_fe)[[unit_var]]
  )
  
  time_fixed_effects = data.table(
    time = names(fixef(mod_fe)[[time_var]]),
    time_effect = fixef(mod_fe)[[time_var]]
  )
  
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
    max_year = max_year,
    pta_type = pta_type,
    enforce_type = enforce_type,
    rho_hat=rho_hat,
    sd_resid=sd_resid,
    time_fe=time_fixed_effects,
    unit_fe=unit_fixed_effects)
  
  # Check for PTA violations and mark groups to drop if violations exist
  stats = rbindlist(pta_enforced$stats)
  stats[, group:=as.numeric(group)]
  stats[, time:=as.numeric(time)]
  
  pta_violations = stats[bound_error == TRUE | (na_error == TRUE & time < 2019)]
  
  if (nrow(pta_violations) > 0) {
    groups_to_drop = unique(pta_violations$group)
    rerun_with_dropped_groups = TRUE  # Mark for rerun with dropped groups
    cat("PTA violations detected. Groups to drop:", paste(groups_to_drop, collapse = ", "), "\n")
  }
  
  # Loop for two passes: first without dropping groups, then with (if violations exist)
  for (run_iteration in 1:(ifelse(rerun_with_dropped_groups, 2, 1))) {
    key = generate_key(unit_var, pta_type, enforce_type, outcome,
                       use_controls, percent_effect, run_iteration)
    
    
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
          max_year = max_year,
          pta_type = pta_type,
          enforce_type = enforce_type,
          rho_hat=rho_hat,
          sd_resid=sd_resid,
          time_fe=time_fixed_effects,
          unit_fe=unit_fixed_effects)
        
        # Merge PTA enforced results with the dataset
        temp = merge(
          dat_clean,
          pta_enforced_sim$df_new,
          by = c(unit_var, group_var, time_var),
          all.x = T,
          suffixes = c('', paste0('_', pta_type, '_', enforce_type)))
        
        new_outcome = paste0(outcome, '_', pta_type, '_', enforce_type)
        temp = temp[!is.na(get(new_outcome))]
        
        # Now scale the counterfactual outcomes within the simulation
        counterfactual_data = copy(temp)
        counterfactual_data[, y_cf := get(new_outcome)]
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
                                   rel_pass_var = rel_pass_var,
                                   max_year = max_year - 1)
          
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
            use_controls = use_controls,
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
          drop_groups = unique(stats[bound_error==TRUE][[group_var]])
          
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
              att = results[[model]]$agg$coeftable[1,1]
              se =  results[[model]]$agg$coeftable[1,2]
            }
            
            
            new_temp[[length(new_temp) + 1]] = data.table(
              model=model,
              level=unit_var,
              outcome = outcome,
              # arr_law = arr_law,
              # noise = noise,
              percent_effect = percent_effect,
              pta_type=pta_type,
              enforce_type=enforce_type,
              controls = use_controls,
              att = att,
              se = se,
              group_list_csa = paste0(results[['cs']]$agg$DIDparams$glist, collapse=' '),
              ng_csa = results[['cs']]$agg$DIDparams$nG,
              ng = length(unique(model_data[[group_var]])),
              n = results[['cs']]$agg$DIDparams$n,
              nT = results[['cs']]$agg$DIDparams$nT,
              ss_csa = nrow(results$csa$agg$DIDparams$data),
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

          returnz = list(results=rbindlist(new_temp), 
                         stats=stats, 
                         te_computed=te_computed)
          return(returnz) 
        } else{
          for(model in names(results)){
            
            new_temp[[length(new_temp) + 1]] = data.table(
              model=model,
              level=unit_var,
              outcome = outcome,
              # arr_law = arr_law,
              # noise = noise,
              percent_effect = percent_effect,
              pta_type=pta_type,
              enforce_type=enforce_type,
              controls = use_controls,
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
          }
          
          
          returnz = list(results=rbindlist(new_temp), 
                         stats=stats, 
                         te_computed=NA)
        }
        
      }
    
    
    final_power = list()
    final_vio = list()
    
    for(i in rez_list){
      spec_n = spec_n + 1
      jr = i$results
      st = i$stats
      jr[, spec_num:=spec_n]
      st[, spec_num:=spec_n]
      final_power[[length(final_power)+ 1]] = jr
      final_vio[[length(final_vio) + 1]] = st
      
    }  
    final_power = rbindlist(final_power)
    
    final_vio = rbindlist(final_vio)
    setnames(final_vio, 'group', 'group_name')
  }
  
  return(list(final_vio=final_vio, final_power=final_power))
}