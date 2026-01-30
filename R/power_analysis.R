# R/power_analysis.R
#' Run Power Analysis with Parallel Trends
#'
#' @param data_clean Clean panel dataset
#' @param unit_var Level of analysis (state or county)
#' @param pta_type Which pta assumption ("cs" or "imputation")
#' @param enforce_type Method for enforcing parallel trends
#' @param outcome Outcome variable
#' @param transform_outcome Character. Outcome transformation: NULL (multiplicative effects, default),
#'   "log" (log transformation), or "ihs" (inverse hyperbolic sine). IHS recommended for outcomes
#'   with zeros (e.g., crime counts). When transformed, PTA violations are not checked.
#' @param controls list of controls
#' @param percent_effect Effect size to simulate
#' @param n_sims Number of simulations
#' @param min_year Numeric. Minimum year to include (optional, default NULL = no minimum)
#' @param max_year Numeric. Maximum year to include (optional, default NULL = no maximum)
#' @param pretrend_test Logical. Whether to compute pre-trend tests (default FALSE)
#' @param outcome_type Character. Type of outcome: "rate" or "count" (default NULL = rate)
#' @param pop_var Character. Population variable name for Poisson models (default NULL)
#' @param family Character. Distribution family for etwfe: NULL (linear) or "poisson" (default NULL)
#' @param trend_type Character. Type of time trend for PTA enforcement: 'common' (default) uses
#'   standard TWFE with common time effects for all cohorts; 'cohort_trend' estimates cohort-specific
#'   polynomial time trends that can extrapolate to post-treatment periods. Ignored for pta_type='cs'.
#' @param trend_order Integer. For trend_type='cohort_trend', the polynomial order (1=linear,
#'   2=quadratic, etc.). Default 1. Ignored when trend_type='common'.
#' @param noise_spec List. Noise engine configuration. Defaults to \code{list(engine = "none")}
#'   for deterministic benchmark. Use \code{list(engine = "iid")} for legacy stochastic behavior.
#'   See \code{\link{normalize_noise_spec}} for full options.
#' @param design_resample Character. Design-level resampling: "none" (default) or
#'   "cluster_bootstrap" (not yet implemented). When engine="none", this is the only way
#'   to get a rejection probability without outcome noise.
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
                               models_to_run=c('cs', 'imputation'),
                               n_sims = 100,
                               min_year = NULL,
                               max_year = NULL,
                               pretrend_test = FALSE,
                               outcome_type = NULL,
                               pop_var = NULL,
                               family = NULL,
                               trend_type = "common",
                               trend_order = 1L,
                               noise_spec = list(engine = "none"),
                               design_resample = "none") {

  # Normalize and validate noise spec
  noise_spec <- normalize_noise_spec(noise_spec)

  # Validate design_resample
  design_resample <- match.arg(design_resample, c("none", "cluster_bootstrap"))
  if (design_resample == "cluster_bootstrap") {
    stop("design_resample='cluster_bootstrap' is not yet implemented. ",
         "Use engine='iid' or engine='ar1' for stochastic power analysis.")
  }

  # Deterministic guard: engine="none" produces identical sims, so n_sims > 1 is waste

  if (noise_spec[["engine"]] == "none" && n_sims > 1L) {
    n_sims <- 1L
    message("engine='none' produces deterministic data; setting n_sims=1. ",
            "To get a rejection probability without outcome noise, use ",
            "design_resample='cluster_bootstrap'.")
  }

  # Check if parallel backend is registered
  # run_power_analysis uses %dopar% for Monte Carlo simulations
  if (!requireNamespace("foreach", quietly = TRUE)) {
    stop("Package 'foreach' is required but not installed. Please install it with: install.packages('foreach')")
  }

  n_workers <- foreach::getDoParWorkers()

  if (n_workers == 1 && n_sims > 1L) {
    warning(
      "\n================================================================================\n",
      "No parallel backend detected. Monte Carlo simulations will run SEQUENTIALLY.\n",
      "This will be very slow for n_sims > 10.\n\n",
      "For faster execution, register a parallel backend BEFORE calling this function:\n\n",
      "  library(doParallel)\n",
      "  cl <- makeCluster(detectCores() - 1)\n",
      "  registerDoParallel(cl)\n",
      "  # ... run your analysis ...\n",
      "  stopCluster(cl)\n\n",
      "See ?registerDoParallel for details.\n",
      "================================================================================\n",
      call. = FALSE,
      immediate. = TRUE
    )
  } else if (n_workers > 1) {
    cat(sprintf("Using %d cores for Monte Carlo simulations (%d total sims)\n",
                n_workers, n_sims))
  }

  units_to_drop = c()
  rerun_with_dropped_units = FALSE

  nrow_full = nrow(data_clean)

  continue = FALSE
  # Filter by year range if specified
  if (!is.null(min_year) && !is.null(max_year)) {
    data_clean_full = data_clean[get(time_var) >= min_year & get(time_var) <= max_year]
  } else if (!is.null(min_year)) {
    data_clean_full = data_clean[get(time_var) >= min_year]
  } else if (!is.null(max_year)) {
    data_clean_full = data_clean[get(time_var) <= max_year]
  } else {
    data_clean_full = data_clean
  }
  data_clean_copy = copy(data_clean_full)
  
  data_clean_copy[, uq_row:=paste(get(unit_var), get(time_var))]

  # Map pta_type to enforce_PTA method
  # "cs" -> "CS", "imputation" -> "imputation", "poisson" -> "poisson"
  enforce_method <- if (pta_type == "cs") "CS" else pta_type

  pta_enforced_orig = enforce_PTA(
    data_clean_copy,
    unit = unit_var,
    group = group_var,
    time = time_var,
    outcome = outcome,
    controls = controls,
    method = enforce_method,
    pop_var = pop_var,
    outcome_type = if (is.null(outcome_type)) "rate" else outcome_type,
    trend_type = trend_type,
    trend_order = trend_order,
    noise_spec = noise_spec
  )

    
  if(is.null(transform_outcome)){
      pta_enforced_orig[, bound_error:= ifelse(counterfactual<0, 1, 0)]
      pta_enforced_orig[bound_error==1, counterfactual:=0]
      pta_enforced_orig[, na_error:=ifelse(is.na(counterfactual), 1, 0)]

      if (noise_spec[["engine"]] == "none") {
        # Deterministic: single-pass violation check
        # Rerunning enforce_PTA won't change anything when noise is off,
        # so we use pta_enforced_orig directly (no second enforce_PTA call).
        max_year_check <- if (!is.null(max_year)) max_year else max(pta_enforced_orig[[time_var]])
        pta_violations <- copy(pta_enforced_orig[bound_error == 1 |
                                (na_error == 1 & get(time_var) < max_year_check)])
        if (nrow(pta_violations) > 0) {
          data_clean_copy <- data_clean_copy[!get(unit_var) %in% pta_violations[[unit_var]]]
          units_to_drop <- sort(unique(pta_violations[[unit_var]]))
        }
        pta_enforced <- pta_enforced_orig

      } else {
        # Stochastic: iterative while loop (re-enforce and re-check until clean)
        while(!continue){
          pta_enforced = enforce_PTA(
            data_clean_copy,
            unit = unit_var,
            group = group_var,
            time = time_var,
            outcome = outcome,
            controls = controls,
            method = enforce_method,
            pop_var = pop_var,
            outcome_type = if (is.null(outcome_type)) "rate" else outcome_type,
            trend_type = trend_type,
            trend_order = trend_order,
            noise_spec = noise_spec
          )

          pta_enforced[, bound_error:= ifelse(counterfactual<0, 1, 0)]
          pta_enforced[bound_error==1, counterfactual:=0]
          pta_enforced[, na_error:=ifelse(is.na(counterfactual), 1, 0)]

          # Only filter by max_year if specified
          max_year_check <- if (!is.null(max_year)) max_year else max(pta_enforced[[time_var]])
          pta_violations = copy(pta_enforced[bound_error == 1 | (na_error == 1 & get(time_var) < max_year_check)])

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

  # Calculate statistics
  n_units_dropped <- length(unique(data_clean_full[[unit_var]])) -
                     length(unique(data_clean_copy[[unit_var]]))
  n_violation_obs <- sum(pta_enforced_orig$bound_error)  # Individual observations with violations
  n_rows_dropped <- nrow(data_clean_full) - nrow(data_clean_copy)  # Total rows dropped (all obs for violating units)

  cat(sprintf("Dataset: %d rows (%d units) -> %d rows (%d units)\n",
              nrow(data_clean_full), length(unique(data_clean_full[[unit_var]])),
              nrow(data_clean_copy), length(unique(data_clean_copy[[unit_var]]))))
  cat(sprintf("PTA violations: %d observations with violations -> %d units dropped -> %d total rows removed\n",
              n_violation_obs, n_units_dropped, n_rows_dropped))
  

  final_power_list = list()
  final_vio_list = list()

  
  # Loop for two passes: first without dropping groups, then with (if violations exist)
  for (run_iteration in 1:1){#:(ifelse(rerun_with_dropped_units, 2, 1))) {
    dat_clean = if (run_iteration == 1) data_clean_full else data_clean_copy
    dat_clean = dat_clean[!is.na(get(outcome))]
    
    rez_list =
      foreach(sim = 1:n_sims,
              .packages = c('data.table', 'fixest', 'did2s',
                            'did', 'didimputation', 'etwfe', 'staggeredpower')) %dopar%
      {
        # CRITICAL: Disable fixest internal threading to prevent race conditions
        # when running multiple simulations per worker. Without this, fepois()
        # causes heap corruption. See: https://github.com/lrberge/fixest/issues/157
        fixest::setFixest_nthreads(1)

        new_temp = list()
        
        # Generate counterfactuals for each simulation
        pta_enforced_sim = enforce_PTA(
          dat_clean,
          unit = unit_var,
          group = group_var,
          time = time_var,
          outcome = outcome,
          controls = controls,
          method = enforce_method,
          pop_var = pop_var,
          outcome_type = if (is.null(outcome_type)) "rate" else outcome_type,
          trend_type = trend_type,
          trend_order = trend_order,
          noise_spec = noise_spec
        )
        # Now scale the counterfactual outcomes within the simulation
        counterfactual_data = copy(pta_enforced_sim)
        counterfactual_data[, y_cf := counterfactual]
        if(!is.null(transform_outcome)){
          if(transform_outcome=='log'){
            counterfactual_data[get(treat_ind_var) == 1, y_cf := y_cf + log(percent_effect)]
          } else if(transform_outcome=='ihs'){
            # Inverse Hyperbolic Sine (IHS) transformation
            # asinh(x) = log(x + sqrt(x^2 + 1))
            # For percent_effect representing multiplicative change (e.g., 1.1 = 10% increase),
            # we apply: asinh(y * percent_effect) - asinh(y) ≈ asinh(percent_effect - 1) for large y
            # But exact form: new_ihs = asinh(exp(asinh(y_cf)) * percent_effect)
            counterfactual_data[get(treat_ind_var) == 1,
                                y_cf := asinh(sinh(y_cf) * percent_effect)]
          }
        } else{
          counterfactual_data[get(treat_ind_var) == 1, y_cf := y_cf * percent_effect]
        }

        # Enforce realistic bounds on counterfactual outcomes
        # Floor at 0: counts/rates cannot be negative
        counterfactual_data[y_cf < 0, y_cf := 0]

        # Ceiling at 1 for share/proportion outcomes (cannot exceed 100%)
        # Detect share outcomes by name (contains "share" case-insensitive)
        if (grepl("share", outcome, ignore.case = TRUE)) {
          counterfactual_data[y_cf > 1, y_cf := 1]
        }

        model_data = copy(counterfactual_data)

        if(sum(model_data[[treat_ind_var]])>0){
          # Model estimation using adapter pattern
          results = estimate_models(
            data = model_data,
            id_var = unit_var,
            outcome_var = 'y_cf',
            time_var = time_var,
            group_var = group_var,
            controls = controls,
            models_to_run = models_to_run,
            cluster_var = unit_var,
            n_cores = 1,  # Already running in parallel across simulations
            pretrend_test = pretrend_test,
            outcome_type = outcome_type,
            pop_var = pop_var,
            family = family
          )
          # drop groups
          if(is.null(transform_outcome)){
            drop_groups = unique(pta_violations[[group_var]])

          } else{
            drop_groups = c()
          }

          for(model in names(results)){
            # Extract from standard_estimate format
            att = results[[model]]$agg$att
            se = results[[model]]$agg$se

            # Compute baseline as pre-period outcome for TREATED units only
            # Different time periods for each method to match their estimation approach

            # Get units that are ever treated
            treated_units <- unique(model_data[get(treat_ind_var) == 1][[unit_var]])

            # y0_bar_csa: Average of treated units at t = g-1 (period immediately before treatment)
            # CSA uses the last pre-treatment period as baseline
            y0_bar_csa <- mean(
              model_data[get(unit_var) %in% treated_units & get(time_var) == get(group_var) - 1]$y_cf,
              na.rm = TRUE
            )

            # y0_bar_imp: Average of treated units across ALL pre-treatment periods (t < g)
            # Imputation uses all pre-periods to estimate fixed effects
            y0_bar_imp <- mean(
              model_data[get(unit_var) %in% treated_units & get(time_var) < get(group_var)]$y_cf,
              na.rm = TRUE
            )

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
              y0_bar_csa = y0_bar_csa,
              y0_bar_imp = y0_bar_imp,
              y0_bar = mean(model_data[!is.na(y_cf)][get(treat_ind_var)==0][[outcome]], na.rm=T),
              sim = sim,
              iteration = run_iteration,
              n_dropped_units = length(units_to_drop),
              share_rows_dropped = share_rows_dropped,
              share_units_dropped = share_units_dropped,
              share_groups_dropped = share_groups_dropped,
              outcome_transformed = ifelse(is.null(transform_outcome), 'No', transform_outcome),
              noise_engine = noise_spec[["engine"]],
              obs_model = noise_spec[["obs_model"]],
              design_resample = design_resample
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
