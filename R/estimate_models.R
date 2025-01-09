# R/estimate_models.R
#' Estimate Difference-in-Differences Models
#' 
#' @param data Panel dataset
#' @param id_var Unit identifier variable
#' @param outcome_var Outcome variable
#' @param arrest_law_type Type of law
#' @param use_controls Whether to include controls
#' @param models_to_run Vector of models to estimate
#' @param event_study Whether to run event study
#' @export
estimate_models <- function(data,
                            id_var,
                            outcome_var,
                            time_var,
                            group_var,
                            arrest_law_type,
                            use_controls,
                            models_to_run,
                            event_study = FALSE) {
  if(is.null(models_to_run)) {
    models_to_run = c('cs', 'imputation', 'sa', 'twfe')
  }
  
  
  # Prepare formula based on controls
  control_formula <- if(use_controls) '~1+unemp_rate+all_crimes_rate' else '~1'
  
  # Select data based on arrest law type
  filtered_data <- switch(arrest_law_type,
                          'all' = data,
                          'mandatory' = data[is_mand==1],
                          'discretionary' = data[is_disc==1])
  
  # Filter data for relevant years
  analysis_data <- filtered_data[between(year, 1995, 2019)]
  
  all_results = list()
  
  

  if('cs' %in% models_to_run){
    # Callaway Sant'Anna estimation
    m_csa = att_gt(yname=outcome_var, xformla=as.formula(control_formula), tname=time_var, idname=id_var,
                   data=analysis_data, allow_unbalanced_panel=FALSE, gname=group_var,
                   panel=TRUE, control_group='notyettreated', clustervars='state', cores=30)
    agg_csa <- aggte(m_csa, type='simple', na.rm=T)
    all_results[['cs']][['agg']] = agg_csa
    
    if(event_study){
      ev_csa <- aggte(m_csa, type='dynamic', na.rm=T, min_e=-6, max_e=10)
      all_results[['cs']][['ev']] = ev_csa
    }
  }
  
  if('twfe' %in% models_to_run){
    # TWFE estimation
    twfe_formula <- paste0(outcome_var, '~1+law_pass', 
                           if(use_controls) '+unemp_rate+index_crimes_rate', '|', id_var, '+year')
    m1_standard <- feols(as.formula(twfe_formula), data=analysis_data, cluster='state')
    
    all_results[['twfe']][['agg']] = m1_standard
    
    # TWFE Event Study estimation
    if(event_study){
      twfe_es_formula <- paste0(outcome_var, '~i(rel_pass, ref = -1, keep=-6:10)', 
                                if(use_controls) '+unemp_rate+index_crimes_rate', '|', id_var, '+year')
      ev_twfe <- feols(as.formula(twfe_es_formula), data=analysis_data, cluster='state')
      all_results[['twfe']][['ev']] = ev_twfe
    }  
    
  }  
  
  
  if('imputation'  %in% models_to_run){
    # DID Imputation estimation
    imp_formula <- if(use_controls) as.formula('~0+unemp_rate+index_crimes_rate') else NULL
    m_imp <- did_imputation(data=analysis_data[!is.na(get(outcome_var))], yname=outcome_var, 
                            gname='year_passed', tname='year',
                            idname=id_var, first_stage=imp_formula, 
                            cluster_var='state')
    
    all_results[['imputation']][['agg']] = m_imp
    
    if(event_study){
      # imp_formula <- if(use_controls) as.formula('~0+unemp_rate+index_crimes_rate') else NULL
      ev_imp <- did_imputation(data=analysis_data[!is.na(get(outcome_var))], yname=outcome_var,
                               gname='year_passed', tname='year',
                               idname=id_var, first_stage=imp_formula, cluster_var='state',
                               horizon=TRUE, pretrends=-6:0)
      all_results[['imputation']][['ev']] = ev_imp
      
    }
  }  
  
  if('etwfe' %in% models_to_run){
    #Extended TWFE
    control_frm <- if(use_controls) '~unemp_rate+all_crimes_rate' else '~1'
    
    m_etwfe = etwfe(
      fml = formula(paste0(outcome_var, control_frm)),
      tvar = time_var,
      gvar = group_var,
      data = analysis_data[!is.na(unemp_rate) & !(is.na(all_crimes_rate))],
      cgroup = 'notyet',
      cluster = 'state'
    )
    
    agg_etwfe = emfx(m_etwfe, type='simple', )
    
    all_results[['etwfe']][['agg']] = agg_etwfe
    
    if(event_study){
      ev_twfe <- emfx(m_etwfe, type='event', post_only=FALSE)
      all_results[['etwfe']][['ev']] = ev_twfe
    }
    
  }
  
  if('sa' %in% models_to_run){
    # Sun Abraham estimation
    # ref -1, nevertreated
    sa_formula <- as.formula(paste0(outcome_var, '~sunab(year_passed, year, ref.p=c(-1))',
                                    if(use_controls) '+unemp_rate+all_crimes_rate', '|year +', id_var))
    m_sa <- feols(sa_formula, data=analysis_data, cluster='state')
    m_sa_summ <- summary(m_sa, agg = 'ATT')
    
    all_results[['sa']][['agg']] = m_sa_summ
    
    if(event_study){
      ev_sa <- summary(m_sa, agg = 'period')
      all_results[['sa']][['ev']] = ev_sa
    }
    
  }  
  
  


  return(all_results)
  
}