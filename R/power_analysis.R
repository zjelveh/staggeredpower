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
                               total_count_var = NULL,
                               family = NULL,
                               trend_type = "common",
                               trend_order = 1L,
                               noise_spec = list(engine = "none"),
                               design_resample = "none",
                               allow_unbalanced_panel = FALSE,
                               est_method = "dr",
                               base_period = "varying") {

  # Normalize and validate noise spec
  noise_spec <- normalize_noise_spec(noise_spec)

  # Count obs model requires pop_var for rate→count conversion
  if (noise_spec$obs_model == "poisson" && is.null(pop_var)) {
    message("obs_model='poisson' requires pop_var; falling back to obs_model='gaussian'")
    noise_spec$obs_model <- "gaussian"
  }

  if (noise_spec$obs_model == "binomial" && is.null(total_count_var)) {
    message("obs_model='binomial' requires total_count_var; falling back to obs_model='gaussian'")
    noise_spec$obs_model <- "gaussian"
  }

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

  # Determine parallel vs sequential execution
  use_parallel <- requireNamespace("foreach", quietly = TRUE)

  if (use_parallel) {
    n_workers <- foreach::getDoParWorkers()
    if (n_workers > 1) {
      cat(sprintf("Using %d cores for Monte Carlo simulations (%d total sims)\n",
                  n_workers, n_sims))
    } else if (n_sims > 1L) {
      message("No parallel backend detected. Running sequentially. ",
              "For faster execution, register a backend:\n",
              "  library(doParallel); cl <- makeCluster(detectCores() - 1); registerDoParallel(cl)")
    }
  } else if (n_sims > 1L) {
    message("Package 'foreach' not installed. Running sequentially. ",
            "For parallel execution: install.packages(c('foreach', 'doParallel'))")
  }

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

  # Map pta_type to enforce_PTA method
  # "cs" -> "CS", "imputation" -> "imputation", "poisson" -> "poisson"
  enforce_method <- if (pta_type == "cs") "CS" else pta_type

  # Count obs model flag: Poisson draw replaces Gaussian noise for additive PTAs
  use_count_obs <- noise_spec$obs_model == "poisson" &&
                   pta_type %in% c("cs", "imputation") &&
                   !is.null(pop_var)

  # Binomial obs model flag: Binomial draw for share/proportion outcomes
  use_binomial_obs <- noise_spec$obs_model == "binomial" &&
                      pta_type %in% c("cs", "imputation") &&
                      !is.null(total_count_var)

  # Initial PTA enforcement (used for bound-error diagnostics only)
  pta_enforced_orig = enforce_PTA(
    copy(data_clean_full),
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

  # --- Track bound errors (diagnostic only, no unit dropping) ---
  n_bound_errors <- 0L
  n_na_errors <- 0L
  violating_units <- character(0)

  if (use_count_obs) {
    # Count obs model: negative counterfactuals structurally impossible
    pta_enforced_orig[, bound_error := 0L]
    pta_enforced_orig[, na_error := 0L]
  } else if (use_binomial_obs) {
    # Binomial obs model: shares bounded [0,1] by construction
    pta_enforced_orig[, bound_error := 0L]
    pta_enforced_orig[, na_error := 0L]
  } else if (is.null(transform_outcome)) {
    pta_enforced_orig[, bound_error := ifelse(counterfactual < 0, 1L, 0L)]
    pta_enforced_orig[bound_error == 1, counterfactual := 0]
    pta_enforced_orig[, na_error := ifelse(is.na(counterfactual), 1L, 0L)]

    n_bound_errors <- sum(pta_enforced_orig$bound_error)
    n_na_errors <- sum(pta_enforced_orig$na_error)
    max_year_check <- if (!is.null(max_year)) max_year else max(pta_enforced_orig[[time_var]])
    violating_units <- unique(pta_enforced_orig[bound_error == 1 |
                        (na_error == 1 & get(time_var) < max_year_check)][[unit_var]])
  }

  n_total_units <- length(unique(data_clean_full[[unit_var]]))

  cat(sprintf("Dataset: %d rows (%d units)\n",
              nrow(data_clean_full), n_total_units))
  cat(sprintf("PTA violations: %d bound errors, %d NA errors, %d units with violations (not dropped)\n",
              n_bound_errors, n_na_errors, length(violating_units)))
  

  dat_clean = data_clean_full[!is.na(get(outcome))]

  sim_fn <- function(sim) {
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

      RATE_SCALE <- 100000

      if (use_count_obs && "cf_mean_rate" %in% names(counterfactual_data)) {
        # --- Count observation model: Poisson draw with pre-draw effect ---
        counterfactual_data[get(treat_ind_var) == 1,
          .lambda := pmax(cf_mean_rate, 0) * get(pop_var) / RATE_SCALE * percent_effect]
        counterfactual_data[get(treat_ind_var) == 1,
          y_cf := stats::rpois(.N, .lambda) / get(pop_var) * RATE_SCALE]
        counterfactual_data[, .lambda := NULL]

      } else if (use_binomial_obs && "cf_mean_rate" %in% names(counterfactual_data)) {
        # --- Binomial observation model: draw count from Binomial(N_total, p_eff) ---
        counterfactual_data[get(treat_ind_var) == 1,
          .p_eff := pmin(pmax(cf_mean_rate, 0) * percent_effect, 1)]
        counterfactual_data[get(treat_ind_var) == 1,
          .n_total := as.integer(round(get(total_count_var)))]
        counterfactual_data[get(treat_ind_var) == 1,
          y_cf := ifelse(.n_total > 0L,
            stats::rbinom(.N, size = pmax(.n_total, 1L), prob = .p_eff) / .n_total,
            0)]
        counterfactual_data[, c(".p_eff", ".n_total") := NULL]

      } else if(!is.null(transform_outcome)){
        if(transform_outcome=='log'){
          counterfactual_data[get(treat_ind_var) == 1, y_cf := y_cf + log(percent_effect)]
        } else if(transform_outcome=='ihs'){
          counterfactual_data[get(treat_ind_var) == 1,
                              y_cf := asinh(sinh(y_cf) * percent_effect)]
        }
      } else{
        counterfactual_data[get(treat_ind_var) == 1, y_cf := y_cf * percent_effect]
      }

      # Enforce realistic bounds on counterfactual outcomes
      # Floor at 0: counts/rates cannot be negative (no-op for Poisson path)
      counterfactual_data[y_cf < 0, y_cf := 0]

      # Ceiling at 1 for share/proportion outcomes (cannot exceed 100%)
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
          family = family,
          allow_unbalanced_panel = allow_unbalanced_panel,
          est_method = est_method,
          base_period = base_period
        )

        for(model in names(results)){
          att = results[[model]]$agg$att
          se = results[[model]]$agg$se

          # Get units that are ever treated
          treated_units <- unique(model_data[get(treat_ind_var) == 1][[unit_var]])

          # y0_bar_csa: Average of treated units at t = g-1
          y0_bar_csa <- mean(
            model_data[get(unit_var) %in% treated_units & get(time_var) == get(group_var) - 1]$y_cf,
            na.rm = TRUE
          )

          # y0_bar_imp: Average of treated units across ALL pre-treatment periods
          y0_bar_imp <- mean(
            model_data[get(unit_var) %in% treated_units & get(time_var) < get(group_var)]$y_cf,
            na.rm = TRUE
          )

          new_temp[[length(new_temp) + 1]] = data.table(
            model = model,
            level = unit_var,
            outcome = outcome,
            percent_effect = percent_effect,
            pta_type = pta_type,
            enforce_type = ifelse(is.null(enforce_type), 'no_controls', paste0(enforce_type, collapse='*')),
            controls = ifelse(is.null(controls), 'no_controls', paste0(controls, collapse='*')),
            att = att,
            se = se,
            n_bound_errors = n_bound_errors,
            n_violating_units = length(violating_units),
            y0_bar_csa = y0_bar_csa,
            y0_bar_imp = y0_bar_imp,
            y0_bar = mean(model_data[!is.na(y_cf)][get(treat_ind_var)==0][[outcome]], na.rm=TRUE),
            sim = sim,
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

  # Dispatch: parallel if foreach available and backend registered, else sequential
  if (use_parallel && foreach::getDoParWorkers() > 1) {
    `%dopar%` <- foreach::`%dopar%`

    rez_list <- foreach::foreach(sim = 1:n_sims,
              .packages = c('data.table', 'staggeredpower')) %dopar% {
      sim_fn(sim)
    }
  } else {
    rez_list <- lapply(1:n_sims, sim_fn)
  }

  final_power_list = list()
  for(i in rez_list) {
    final_power_list[[length(final_power_list)+1]] = i$results
  }

  final_power = rbindlist(final_power_list)

  # Build violation summary from initial enforcement
  if (is.null(transform_outcome) && (n_bound_errors > 0 || n_na_errors > 0)) {
    max_year_check <- if (!is.null(max_year)) max_year else max(pta_enforced_orig[[time_var]])
    final_vio <- pta_enforced_orig[bound_error == 1 |
                    (na_error == 1 & get(time_var) < max_year_check)]
    final_vio <- final_vio[, .(counterfactual, bound_error, na_error,
                               level = unit_var,
                               unit = get(unit_var),
                               group = get(group_var),
                               time = get(time_var),
                               outcome,
                               pta_type,
                               controls = ifelse(is.null(controls), 'no_controls', paste0(controls, collapse='*')),
                               enforce_type = ifelse(is.null(enforce_type), 'no_controls', paste0(enforce_type, collapse='*')))]
  } else {
    final_vio <- NA
  }

  return(list(final_vio = final_vio,
              final_power = final_power))
}
