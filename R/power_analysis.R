# R/power_analysis.R
#' Run Power Analysis with Parallel Trends
#'
#' @param data_clean Clean panel dataset
#' @param unit_var Level of analysis (state or county)
#' @param group_var Character. Name of the treatment group/cohort column
#' @param time_var Character. Name of the time period column
#' @param rel_pass_var Character. Name of the relative time to treatment column
#' @param treat_ind_var Character. Name of the treatment indicator column
#' @param pta_type Which pta assumption ("cs" or "imputation")
#' @param enforce_type Method for enforcing parallel trends
#' @param outcome Outcome variable
#' @param transform_outcome Character. Outcome transformation: NULL (multiplicative effects, default),
#'   "log" (log transformation), or "ihs" (inverse hyperbolic sine). IHS recommended for outcomes
#'   with zeros (e.g., crime counts). When transformed, PTA violations are not checked.
#' @param controls list of controls
#' @param percent_effect Effect size to simulate
#' @param models_to_run Character vector. Models to estimate (default c('cs', 'imputation'))
#' @param n_sims Number of simulations
#' @param min_year Numeric. Minimum year to include (optional, default NULL = no minimum)
#' @param max_year Numeric. Maximum year to include (optional, default NULL = no maximum)
#' @param pretrend_test Logical. Whether to compute pre-trend tests (default FALSE)
#' @param outcome_type Character. Type of outcome: "rate" or "count" (default NULL = rate)
#' @param pop_var Character. Population variable name for Poisson models (default NULL)
#' @param total_count_var Character. Total count variable for binomial models (default NULL)
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
#' @param allow_unbalanced_panel Logical. Allow unbalanced panels (default FALSE)
#' @param est_method Character. Estimation method for CS estimator (default "dr")
#' @param base_period Character. Base period for CS estimator (default "varying")
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
                               base_period = "varying",
                               wcr_models = NULL,
                               wcr_B = 999L,
                               wcr_weights = c("rademacher", "webb")) {
  wcr_weights <- match.arg(wcr_weights)

  # Normalize and validate noise spec
  noise_spec <- normalize_noise_spec(noise_spec)

  # Reroll control-mean surface defaults to MATCH the DGP's parallel-trends assumption
  # (unless the caller set noise_spec$control_mean explicitly). The CS DGP moves the
  # controls along a single calendar-time path fit to did's OWN control-side dc(g,t)
  # ("cs_group"), so CS's comparison and the DGP agree and the CS null bias is small
  # (a global FE surface, "cs_anchor", instead follows the all-states average and
  # leaves a larger residual). Imputation uses the additive FE surface it imputes
  # with ("twfe"). Only relevant when noise_spec$reroll_controls is TRUE.
  if (is.null(noise_spec$control_mean)) {
    noise_spec$control_mean <- if (identical(pta_type, "cs")) "cs_group" else "twfe"
  }

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
    data.table::copy(data_clean_full),
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
    noise_spec = noise_spec,
    control_group = "notyettreated",
    est_method = est_method,
    base_period = base_period,
    allow_unbalanced_panel = allow_unbalanced_panel
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
        noise_spec = noise_spec,
        control_group = "notyettreated",
        est_method = est_method,
        base_period = base_period,
        allow_unbalanced_panel = allow_unbalanced_panel
      )
      # Now scale the counterfactual outcomes within the simulation
      counterfactual_data = data.table::copy(pta_enforced_sim)
      counterfactual_data[, y_cf := counterfactual]

      RATE_SCALE <- 100000

      if (isTRUE(noise_spec$reroll_controls) &&
          (use_count_obs || use_binomial_obs) && "cf_mean_rate" %in% names(counterfactual_data)) {
        # --- RE-ROLL CONTROLS: harmonized treated mean (cf_mean_rate) + modular
        #     control-mean surface, then bounded noise (mean-preserving latent lognormal
        #     x count/share draw) on EVERY cell so control-side sampling variance enters
        #     the estimator. Opt-in via noise_spec$reroll_controls. ---
        cm_fn  <- get_control_mean_surface(noise_spec$control_mean)
        mu_all <- cm_fn(counterfactual_data, unit_var, time_var, outcome, treat_ind_var, pop_var)
        .is_ctrl <- counterfactual_data[[treat_ind_var]] != 1
        counterfactual_data[, .mean_rate := cf_mean_rate]
        counterfactual_data[.is_ctrl, .mean_rate := mu_all[.is_ctrl]]
        counterfactual_data[, .eff := data.table::fifelse(get(treat_ind_var) == 1, percent_effect, 1)]
        sig <- if (!is.null(noise_spec$latent_sigma)) noise_spec$latent_sigma else
               latent_sigma_default(counterfactual_data, outcome, pop_var, treat_ind_var)
        rho_lat <- if (!is.null(noise_spec$latent_rho)) noise_spec$latent_rho else 0
        # Standardized latent Gaussian .v ~ marginal N(0,1) (rho_lat adds serial dependence,
        # not magnitude): stationary var = 1; innovation var = 1 - rho^2. It feeds BOTH the
        # multiplicative lognormal shock exp(sig*.v - sig^2/2) (rate / legacy share) AND the
        # Gaussian copula pnorm(.v) for the Beta-Binomial share draw.
        if (rho_lat == 0) {
          counterfactual_data[, .v := stats::rnorm(.N, 0, 1)]
        } else {
          data.table::setorderv(counterfactual_data, c(unit_var, time_var))
          counterfactual_data[, .v := {
            n <- .N; z <- numeric(n); z[1] <- stats::rnorm(1, 0, 1)
            if (n > 1) { e <- stats::rnorm(n, 0, sqrt(1 - rho_lat^2))
                        for (j in 2:n) z[j] <- rho_lat * z[j - 1] + e[j] }
            z
          }, by = c(unit_var)]
        }
        if (use_count_obs) {
          # Poisson-lognormal: mean-preserving multiplicative shock exp(sig*.v - sig^2/2)
          counterfactual_data[, .lambda := pmax(.mean_rate, 0) * .eff * get(pop_var) / RATE_SCALE *
                                exp(sig * .v - sig^2 / 2)]
          counterfactual_data[, y_cf := stats::rpois(.N, pmax(.lambda, 1e-8)) / get(pop_var) * RATE_SCALE]
        } else if (identical(noise_spec$share_od, "betabinom")) {
          # Beta-Binomial: p ~ Beta(mu*phi, (1-mu)*phi) -- mean-preserving (E[p]=mu), bounded on
          # (0,1) BY CONSTRUCTION (no clamp in either direction), serially dependent via the
          # Gaussian copula pnorm(.v). Concentration phi = latent_phi. Degenerate boundary means
          # (mu<=0 / mu>=1) resolve to a certain 0 / 1 -- the exact Beta limits, not a clamp
          # (the pmax on the shape args only keeps qbeta finite for those rows, which fifelse
          # then overrides; for every in-support cell 0<mu<1 it is a no-op).
          phi <- if (!is.null(noise_spec$latent_phi)) noise_spec$latent_phi else 1e6
          counterfactual_data[, .n_total := as.integer(round(get(total_count_var)))]
          counterfactual_data[, .mu := .mean_rate * .eff]
          counterfactual_data[, .p_eff := {
            p <- stats::qbeta(stats::pnorm(.v), pmax(.mu, 1e-9) * phi, pmax(1 - .mu, 1e-9) * phi)
            data.table::fifelse(.mu <= 0, 0, data.table::fifelse(.mu >= 1, 1, p))
          }]
          counterfactual_data[, y_cf := data.table::fifelse(.n_total > 0L,
              stats::rbinom(.N, size = pmax(.n_total, 1L), prob = .p_eff) / .n_total, 0)]
        } else {
          # legacy multiplicative-lognormal share: p = mu * exp(sig*.v - sig^2/2), capped at 1
          counterfactual_data[, .n_total := as.integer(round(get(total_count_var)))]
          counterfactual_data[, .p_eff := pmin(pmax(.mean_rate, 0) * .eff * exp(sig * .v - sig^2 / 2), 1)]
          counterfactual_data[, y_cf := data.table::fifelse(.n_total > 0L,
              stats::rbinom(.N, size = pmax(.n_total, 1L), prob = .p_eff) / .n_total, 0)]
        }
        drop_cols <- intersect(c(".mean_rate", ".eff", ".v", ".mu", ".lat", ".lambda", ".n_total", ".p_eff"),
                               names(counterfactual_data))
        if (length(drop_cols)) counterfactual_data[, (drop_cols) := NULL]

      } else if (use_count_obs && "cf_mean_rate" %in% names(counterfactual_data)) {
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

      model_data = data.table::copy(counterfactual_data)

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

          # Modular WCR-t inference: after the estimator runs, apply the wild cluster
          # bootstrap to any model the user flagged via wcr_models. Cheap for imputation;
          # expensive for CS (att_gt x B) -- opt in per estimator.
          wcr_cval <- NA_real_; wcr_reject <- NA_integer_; wcr_pval <- NA_real_
          if (!is.null(wcr_models) && model %in% wcr_models) {
            .wr <- tryCatch(wcr_test(
                     data = model_data, outcome = "y_cf", unit_var = unit_var,
                     time_var = time_var, group_var = group_var, treat_ind_var = treat_ind_var,
                     estimator = model, cluster_var = unit_var, B = wcr_B, weights = wcr_weights,
                     est_method = est_method, base_period = base_period,
                     allow_unbalanced_panel = allow_unbalanced_panel),
                   error = function(e) NULL)
            if (!is.null(.wr)) { wcr_cval <- .wr$wcr_cval; wcr_reject <- .wr$reject; wcr_pval <- .wr$wcr_pval }
          }

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
            wcr_cval = wcr_cval,
            wcr_reject = wcr_reject,
            wcr_pval = wcr_pval,
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

        returnz = list(results=data.table::rbindlist(new_temp))
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

  final_power = data.table::rbindlist(final_power_list)

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
