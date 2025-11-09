# R/estimate_models.R
#' Estimate Difference-in-Differences Models
#'
#' @param data Panel dataset
#' @param id_var Unit identifier variable
#' @param outcome_var Outcome variable
#' @param controls Whether to include controls
#' @param models_to_run Vector of models to estimate
#' @param event_study Whether to run event study
#' @param min_year Numeric. Minimum year to include (optional)
#' @param max_year Numeric. Maximum year to include (optional)
#' @export
estimate_models <- function(data,
                            id_var,
                            outcome_var,
                            time_var,
                            group_var,
                            controls,
                            models_to_run,
                            cluster_var = NULL,
                            treat_ind_var='law_pass',
                            event_study = FALSE,
                            min_year = NULL,
                            max_year = NULL,
                            n_cores = NULL) {
  # Default cluster_var to id_var if not specified
  if(is.null(cluster_var)) {
    cluster_var <- id_var
  }

  # Default n_cores to all available - 1
  if(is.null(n_cores)) {
    n_cores <- max(1, parallel::detectCores() - 1)
  }
  if(is.null(models_to_run)) {
    models_to_run = c('cs', 'imputation', 'sa', 'twfe')
  }

  # Filter by year range if specified
  if (!is.null(min_year) && !is.null(max_year)) {
    analysis_data <- data[get(time_var) >= min_year & get(time_var) <= max_year]
  } else if (!is.null(min_year)) {
    analysis_data <- data[get(time_var) >= min_year]
  } else if (!is.null(max_year)) {
    analysis_data <- data[get(time_var) <= max_year]
  } else {
    analysis_data <- data
  }
  
  all_results = list()
  
  
  if('cs' %in% models_to_run){
    # Estimate FEs with controls if provided
    if(length(controls) > 0){
      # Create formula with all controls
      control_formula = as.formula(paste0(" ~ ", paste(controls, collapse = " + ")))
    } else {
      control_formula = as.formula(" ~ 1")
    }
    
    # Callaway Sant'Anna estimation
    m_csa = att_gt(yname=outcome_var,
                   xformla=control_formula,
                   tname=time_var,
                   idname=id_var,
                   data=analysis_data,
                   allow_unbalanced_panel=FALSE,
                   gname=group_var,
                   panel=TRUE,
                   est_method='dr',
                   control_group='notyettreated',
                   clustervars=cluster_var,
                   cores=n_cores)
    agg_csa <- aggte(m_csa, type='simple', na.rm=T)
    all_results[['cs']][['agg']] = agg_csa
    
    if(event_study){
      ev_csa <- aggte(m_csa, type='dynamic', na.rm=T, min_e=-6, max_e=10)
      all_results[['cs']][['ev']] = ev_csa
    }

    # ###reg version
    # # Callaway Sant'Anna estimation
    # m_csa_reg = att_gt(yname=outcome_var, 
    #                xformla=control_formula, 
    #                tname=time_var, 
    #                idname=id_var,
    #                data=analysis_data,
    #                allow_unbalanced_panel=FALSE, 
    #                gname=group_var,
    #                panel=TRUE, 
    #                est_method='reg',
    #                control_group='notyettreated',
    #                clustervars=cluster_var,
    #                cores=30)
    # agg_csa_reg <- aggte(m_csa_reg, type='simple', na.rm=T)
    # all_results[['cs_reg']][['agg']] = agg_csa_reg
    
  }
  
  if('twfe' %in% models_to_run){
    # Estimate FEs with controls if provided
    if(length(controls) > 0){
      # Create formula with all controls
      twfe_formula = as.formula(
        paste0(outcome_var, '~', treat_ind_var, 
               paste(controls, collapse = " + "),
               '|', 
               id_var, '+', time_var))
    } else {
      # Create formula with all controls
      twfe_formula = as.formula(
        paste0(outcome_var, '~', treat_ind_var, '|',
               id_var, '+', time_var))
    }
    
    # TWFE estimation
    m1_standard <- feols(twfe_formula, data=analysis_data, cluster=cluster_var)
    
    all_results[['twfe']][['agg']] = m1_standard
    
    # TWFE Event Study estimation
    if(event_study){
      # Estimate FEs with controls if provided
      if(length(controls) > 0){
        # Create formula with all controls
        twfe_formula = as.formula(
          paste0(outcome_var, '~i(rel_pass, ref = -1, keep=-6:10)+', 
                 paste(controls, collapse = " + "),
                 '|', 
                 id_var, '+', time_var))
      } else {
        # Create formula with all controls
        twfe_formula = as.formula(
          paste0(outcome_var, '~i(rel_pass, ref = -1, keep=-6:10) |',
                 id_var, '+', time_var))
      }
      
      ev_twfe <- feols(twfe_es_formula, data=analysis_data, cluster=cluster_var)
      all_results[['twfe']][['ev']] = ev_twfe
    }  
    
  }  
  
  
  if('imputation'  %in% models_to_run){
    # DID Imputation estimation
    # Estimate FEs with controls if provided
    if(length(controls) > 0){
      # Create formula with all controls
      imp_formula = as.formula(
        paste0('~ 0 + ',
               paste(controls, collapse = " + ")))
    } else {
      # Create formula with all controls
      imp_formula = NULL
    }
    
    m_imp <- did_imputation(data=analysis_data[!is.na(get(outcome_var))], 
                            yname=outcome_var, 
                            gname=group_var, 
                            tname=time_var,
                            idname=id_var, 
                            first_stage=imp_formula, 
                            cluster_var=cluster_var)

    all_results[['imputation']][['agg']] = m_imp
    
    if(event_study){
      ev_imp <- did_imputation(data=analysis_data[!is.na(get(outcome_var))], 
                               yname=outcome_var,
                               gname=group_var,
                               tname=time_var,
                               idname=id_var, 
                               first_stage=imp_formula, 
                               cluster_var=cluster_var,
                               horizon=TRUE, 
                               pretrends=-6:0)
      all_results[['imputation']][['ev']] = ev_imp
      
    }
  }  
  

  return(all_results)
  
}