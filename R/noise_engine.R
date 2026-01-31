# R/noise_engine.R
# Shared noise engine for staggered DiD power analysis counterfactual generation.
#
# Provides configurable noise with three modes:
#   engine = "none"        — deterministic (eps = 0)
#   engine = "iid"         — reproduce legacy per-PTA behavior
#   engine = "ar1"         — AR(1) idiosyncratic process per unit
#   engine = "ar1_common"    — AR(1) idiosyncratic + common calendar-year shocks
#   engine = "ar1_anchored"  — AR(1) idiosyncratic + ATT-weighted anchored common
#                               (additive: centered common shocks; log: no common + Jensen correction)
#
# Each PTA family has its own calibration function that extracts residuals
# on the appropriate scale (additive for imputation/CS, log for Poisson).
# The draw functions then generate noise paths using the calibrated parameters.

#' Normalize and validate a noise specification
#'
#' @param noise_spec List with noise configuration. NULL uses defaults.
#' @return Validated noise_spec list with all fields populated.
#' @export
normalize_noise_spec <- function(noise_spec = NULL) {
  if (is.null(noise_spec)) noise_spec <- list()

  defaults <- list(
    engine     = "iid",
    innovation = "normal",
    common_shock = FALSE,
    rho        = NULL,
    cs_pool    = "global",
    obs_model  = NULL
  )

  for (nm in names(defaults)) {
    if (is.null(noise_spec[[nm]])) noise_spec[[nm]] <- defaults[[nm]]
  }

  # Auto-set obs_model based on engine
  if (is.null(noise_spec$obs_model) || identical(noise_spec$obs_model, "auto")) {
    noise_spec$obs_model <- if (noise_spec$engine == "none") "deterministic" else "gaussian"
  }

  # Validate
  valid_engines <- c("none", "iid", "ar1", "ar1_common", "ar1_anchored")
  if (!noise_spec$engine %in% valid_engines) {
    stop(sprintf("noise_spec$engine must be one of: %s. Got: '%s'",
                 paste(valid_engines, collapse = ", "), noise_spec$engine))
  }
  if (!noise_spec$innovation %in% c("normal", "empirical")) {
    stop(sprintf("noise_spec$innovation must be 'normal' or 'empirical'. Got: '%s'",
                 noise_spec$innovation))
  }
  if (!noise_spec$cs_pool %in% c("global", "cohort")) {
    stop(sprintf("noise_spec$cs_pool must be 'global' or 'cohort'. Got: '%s'",
                 noise_spec$cs_pool))
  }
  if (!noise_spec$obs_model %in% c("deterministic", "gaussian", "poisson")) {
    stop(sprintf("noise_spec$obs_model must be 'deterministic', 'gaussian', or 'poisson'. Got: '%s'",
                 noise_spec$obs_model))
  }
  if (!is.null(noise_spec$rho)) {
    if (!is.numeric(noise_spec$rho) || noise_spec$rho < 0 || noise_spec$rho > 0.99) {
      stop("noise_spec$rho must be numeric in [0, 0.99] or NULL (auto-estimate)")
    }
  }

  noise_spec
}


# ---------------------------------------------------------------------------
# Calibration functions
# ---------------------------------------------------------------------------

#' Calibrate noise parameters from imputation (TWFE) model
#'
#' Extracts residual structure from untreated data to calibrate the noise engine.
#' Works on additive (level) scale.
#'
#' @param mod_fe Fitted feols model from untreated data
#' @param df_untreated data.table of untreated observations (must include unit and time columns)
#' @param unit_col Character. Unit column name.
#' @param time_col Character. Time column name.
#' @param noise_spec Normalized noise_spec list.
#' @return Calibration list with: scale, engine, sigma, rho, sigma_eta, eta_pool, u_pool
#' @export
calibrate_noise_imputation <- function(mod_fe, df_untreated, unit_col, time_col, noise_spec) {
  engine <- noise_spec$engine

  calib <- list(
    scale  = "additive",
    engine = engine,
    sigma  = sigma(mod_fe)
  )

  if (engine %in% c("ar1", "ar1_common", "ar1_anchored")) {
    # Compute residuals on untreated
    df_untreated[, .noise_e_hat := resid(mod_fe)]

    # Order and create lag
    data.table::setorderv(df_untreated, c(unit_col, time_col))
    df_untreated[, .noise_e_lag := shift(.noise_e_hat, 1L), by = c(unit_col)]

    # Estimate rho
    complete <- df_untreated[!is.na(.noise_e_lag)]
    if (nrow(complete) > 5) {
      rho_hat <- stats::coef(stats::lm(.noise_e_hat ~ 0 + .noise_e_lag, data = complete))[[1]]
      rho_hat <- max(0, min(rho_hat, 0.95))
    } else {
      rho_hat <- 0
    }
    if (!is.null(noise_spec$rho)) rho_hat <- noise_spec$rho
    calib$rho <- rho_hat

    # Innovations
    complete[, .noise_eta := .noise_e_hat - rho_hat * .noise_e_lag]
    if (noise_spec$innovation == "normal") {
      calib$sigma_eta <- stats::sd(complete$.noise_eta, na.rm = TRUE)
    } else {
      calib$eta_pool <- complete$.noise_eta[!is.na(complete$.noise_eta)]
    }

    # Common shock: resample time-FE innovations
    if (engine == "ar1_common" || isTRUE(noise_spec$common_shock)) {
      gamma <- fixest::fixef(mod_fe)[[time_col]]
      gamma_dt <- data.table::data.table(
        time = as.numeric(names(gamma)),
        gamma = as.numeric(gamma)
      )
      data.table::setorder(gamma_dt, time)
      gamma_dt[, dgamma := gamma - shift(gamma, 1L)]
      calib$u_pool <- gamma_dt[!is.na(dgamma), dgamma]
    }

    # Per-year control residuals for anchored common shocks
    if (engine == "ar1_anchored") {
      control_resid_by_year <- list()
      for (yr in sort(unique(df_untreated[[time_col]]))) {
        resids_yr <- df_untreated[get(time_col) == yr, .noise_e_hat]
        resids_yr <- resids_yr[!is.na(resids_yr)]
        if (length(resids_yr) > 0) {
          control_resid_by_year[[as.character(yr)]] <- resids_yr
        }
      }
      calib$control_resid_by_year <- control_resid_by_year
    }

    # Cleanup temp columns
    df_untreated[, c(".noise_e_hat", ".noise_e_lag") := NULL]
    if (".noise_eta" %in% names(complete)) {
      # complete is a reference subset; clean from df_untreated
      if (".noise_eta" %in% names(df_untreated)) {
        df_untreated[, .noise_eta := NULL]
      }
    }
  }

  calib
}


#' Calibrate noise parameters from Poisson model (log scale)
#'
#' @param mod_pois Fitted fepois model from untreated data
#' @param df_untreated data.table of untreated observations
#' @param working_outcome Character. Name of the working outcome column (count).
#' @param pop_var Character. Name of population variable.
#' @param unit_col Character. Unit column name.
#' @param time_col Character. Time column name.
#' @param noise_spec Normalized noise_spec list.
#' @return Calibration list with: scale, engine, sigma_log, rho, sigma_eta, eta_pool, u_pool, obs_model
#' @export
calibrate_noise_poisson <- function(mod_pois, df_untreated, working_outcome,
                                     pop_var, unit_col, time_col, noise_spec) {
  engine <- noise_spec$engine
  c0 <- 0.5  # regularization constant for log of zero counts

  calib <- list(
    scale     = "log",
    engine    = engine,
    obs_model = noise_spec$obs_model
  )

  if (engine == "none") return(calib)

  # Compute fitted lambda for untreated to get log-scale residuals
  # We need unit and time effects to reconstruct fitted values
  unit_fe_vals <- fixest::fixef(mod_pois)[[unit_col]]
  time_fe_vals <- fixest::fixef(mod_pois)[[time_col]]

  # Build lookup tables
  unit_names <- if (is.numeric(df_untreated[[unit_col]])) as.numeric(names(unit_fe_vals)) else names(unit_fe_vals)
  time_names <- if (is.numeric(df_untreated[[time_col]])) as.numeric(names(time_fe_vals)) else names(time_fe_vals)

  unit_dt <- data.table::data.table(unit = unit_names, .pois_unit_fe = as.numeric(unit_fe_vals))
  data.table::setnames(unit_dt, "unit", unit_col)
  time_dt <- data.table::data.table(time = time_names, .pois_time_fe = as.numeric(time_fe_vals))
  data.table::setnames(time_dt, "time", time_col)

  df_untreated[unit_dt, on = unit_col, .pois_unit_fe := i..pois_unit_fe]
  df_untreated[time_dt, on = time_col, .pois_time_fe := i..pois_time_fe]

  # Add control/trend effects if model has non-FE coefficients
  model_coefs <- stats::coef(mod_pois)
  df_untreated[, .pois_log_rate := .pois_unit_fe + .pois_time_fe]
  if (length(model_coefs) > 0) {
    for (nm in names(model_coefs)) {
      if (nm %in% names(df_untreated) && !is.na(model_coefs[nm])) {
        df_untreated[, .pois_log_rate := .pois_log_rate + get(nm) * model_coefs[nm]]
      }
    }
  }

  df_untreated[, .pois_lambda_hat := exp(.pois_log_rate) * get(pop_var)]

  # Log-scale residual
  df_untreated[, .noise_e_hat := log(get(working_outcome) + c0) - log(.pois_lambda_hat + c0)]

  # For iid: just compute sigma
  calib$sigma_log <- stats::sd(df_untreated$.noise_e_hat, na.rm = TRUE)

  if (engine %in% c("ar1", "ar1_common", "ar1_anchored")) {
    # Order and create lag
    data.table::setorderv(df_untreated, c(unit_col, time_col))
    df_untreated[, .noise_e_lag := shift(.noise_e_hat, 1L), by = c(unit_col)]

    # Estimate rho
    complete <- df_untreated[!is.na(.noise_e_lag) & !is.na(.noise_e_hat)]
    if (nrow(complete) > 5) {
      rho_hat <- stats::coef(stats::lm(.noise_e_hat ~ 0 + .noise_e_lag, data = complete))[[1]]
      rho_hat <- max(0, min(rho_hat, 0.95))
    } else {
      rho_hat <- 0
    }
    if (!is.null(noise_spec$rho)) rho_hat <- noise_spec$rho
    calib$rho <- rho_hat

    # Innovations
    complete[, .noise_eta := .noise_e_hat - rho_hat * .noise_e_lag]
    if (noise_spec$innovation == "normal") {
      calib$sigma_eta <- stats::sd(complete$.noise_eta, na.rm = TRUE)
    } else {
      calib$eta_pool <- complete$.noise_eta[!is.na(complete$.noise_eta)]
    }

    # Common shock on log scale
    if (engine == "ar1_common" || isTRUE(noise_spec$common_shock)) {
      # Time means of log-scale residuals, then difference
      u_means <- df_untreated[!is.na(.noise_e_hat),
                               .(u_mean = mean(.noise_e_hat, na.rm = TRUE)),
                               by = c(time_col)]
      data.table::setorderv(u_means, time_col)
      u_means[, du := u_mean - shift(u_mean, 1L)]
      calib$u_pool <- u_means[!is.na(du), du]
    }

    # Cleanup
    if (".noise_e_lag" %in% names(df_untreated)) df_untreated[, .noise_e_lag := NULL]
  }

  # Cleanup temp columns
  temp_cols <- c(".pois_unit_fe", ".pois_time_fe", ".pois_log_rate",
                 ".pois_lambda_hat", ".noise_e_hat")
  existing <- intersect(temp_cols, names(df_untreated))
  if (length(existing) > 0) df_untreated[, (existing) := NULL]

  # Jensen correction flag for any stochastic log-scale engine
  if (engine %in% c("ar1", "ar1_common", "ar1_anchored", "iid")) {
    calib$jensen_correction <- TRUE
  }

  calib
}


#' Calibrate noise parameters for CS (Callaway-Sant'Anna) PTA
#'
#' Computes one-step innovation residuals from control long-difference regressions.
#' Also stores per-(g,rp) residual SD for iid backward compatibility.
#'
#' @param df data.table with panel data
#' @param unit_col Character. Unit column name.
#' @param group_col Character. Group (cohort) column name.
#' @param time_col Character. Time column name.
#' @param outcome_col Character. Outcome column name.
#' @param controls Character vector. Control variable names (or NULL).
#' @param noise_spec Normalized noise_spec list.
#' @return Calibration list with: scale, engine, rho, sigma_eta, eta_pool, u_pool,
#'   resid_sd_by_cell (for iid backward compat)
#' @export
calibrate_noise_cs <- function(df, unit_col, group_col, time_col, outcome_col,
                                controls, noise_spec) {
  engine <- noise_spec$engine

  calib <- list(
    scale  = "additive",
    engine = engine
  )

  if (engine == "none") return(calib)

  groups <- sort(unique(df[[group_col]]))
  max_year <- max(df[[time_col]])

  # Storage for per-cell resid_sd (iid backward compat)
  resid_sd_by_cell <- list()

  # Storage for one-step innovations (for ar1/ar1_common)
  all_onestep <- list()    # data.table rows: unit, cohort, calendar_year, r_tilde
  all_common <- list()     # data.table rows: cohort, calendar_year, u_k

  # Build keyed lookup for outcomes
  y_lookup <- df[, c(unit_col, time_col, outcome_col), with = FALSE]
  data.table::setkeyv(y_lookup, c(unit_col, time_col))

  for (g in groups) {
    pre_time <- g - 1
    max_rel <- max(df[get(group_col) == g][[time_col]], na.rm = TRUE) - g

    # Storage for cumulative long-diff residuals per control state
    # resid_ld[s] at k is the cumulative residual for control state s at horizon k
    prev_resid_ld <- list()  # keyed by unit, stores cumulative resid at previous k

    for (rp in 0:max_rel) {
      curr_time <- g + rp
      if (curr_time > max_year) next

      # Control pool: not-yet-treated at curr_time OR never-treated
      control_data <- df[(get(group_col) > curr_time & get(group_col) <= max_year) | is.na(get(group_col))]

      # Control long-diffs
      control_pre <- control_data[get(time_col) == pre_time]
      control_post <- control_data[get(time_col) == curr_time]

      control_changes <- merge(control_pre, control_post,
                                by = c(unit_col),
                                suffixes = c("_pre", "_post"))
      control_changes[, delta_y := get(paste0(outcome_col, "_post")) -
                        get(paste0(outcome_col, "_pre"))]

      if (nrow(control_changes) == 0) next

      # Build regression matrix
      if (!is.null(controls) && length(controls) > 0) {
        X_control <- as.matrix(cbind(1, control_changes[, paste0(controls, "_pre"), with = FALSE]))
      } else {
        X_control <- as.matrix(rep(1, nrow(control_changes)))
      }

      # Fit regression
      reg_model <- fastglm::fastglm(
        x = X_control,
        y = control_changes$delta_y,
        family = stats::gaussian(link = "identity")
      )

      # Per-cell residual SD (for iid backward compat)
      if (reg_model$df.residual == 0) {
        cell_resid_sd <- sqrt(sum(reg_model$residuals^2) / 1)
      } else {
        cell_resid_sd <- sqrt(sum(reg_model$residuals^2) / reg_model$df.residual)
      }
      resid_sd_by_cell[[paste(g, rp, sep = "_")]] <- cell_resid_sd

      # For ar1/ar1_common/ar1_anchored: compute one-step innovations from long-diff residuals
      if (engine %in% c("ar1", "ar1_common", "ar1_anchored")) {
        # Cumulative long-diff residuals for this cell
        resid_ld_curr <- data.table::data.table(
          unit = control_changes[[unit_col]],
          resid_ld = reg_model$residuals
        )

        # One-step innovation: r_tilde = resid_ld(k) - resid_ld(k-1)
        # For rp=0 (k=g): resid_ld(g-1) = 0 by definition
        if (rp == 0) {
          resid_ld_curr[, r_tilde := resid_ld]
        } else {
          # Merge with previous cumulative residuals
          prev_names <- names(prev_resid_ld[[as.character(g)]])
          prev_vals <- as.numeric(prev_resid_ld[[as.character(g)]])
          # Match type of unit column
          if (is.numeric(resid_ld_curr$unit)) {
            prev_names <- as.numeric(prev_names)
          }
          prev_dt <- data.table::data.table(
            unit = prev_names,
            prev_resid = prev_vals
          )
          resid_ld_curr <- merge(resid_ld_curr, prev_dt, by = "unit", all.x = TRUE)
          resid_ld_curr[is.na(prev_resid), prev_resid := 0]
          resid_ld_curr[, r_tilde := resid_ld - prev_resid]
        }

        # Store cumulative residuals for next iteration
        if (is.null(prev_resid_ld[[as.character(g)]])) {
          prev_resid_ld[[as.character(g)]] <- stats::setNames(
            resid_ld_curr$resid_ld, as.character(resid_ld_curr$unit)
          )
        } else {
          # Update with current values (some control units may change across periods)
          for (j in seq_len(nrow(resid_ld_curr))) {
            prev_resid_ld[[as.character(g)]][as.character(resid_ld_curr$unit[j])] <-
              resid_ld_curr$resid_ld[j]
          }
        }

        # Store one-step innovations
        all_onestep[[length(all_onestep) + 1]] <- data.table::data.table(
          unit = resid_ld_curr$unit,
          cohort = g,
          calendar_year = curr_time,
          r_tilde = resid_ld_curr$r_tilde
        )

        # Common shock: cross-sectional mean of one-step innovations
        if (engine %in% c("ar1_common", "ar1_anchored") || isTRUE(noise_spec$common_shock)) {
          all_common[[length(all_common) + 1]] <- data.table::data.table(
            cohort = g,
            calendar_year = curr_time,
            u_k = mean(resid_ld_curr$r_tilde, na.rm = TRUE)
          )
        }
      }
    }
  }

  calib$resid_sd_by_cell <- resid_sd_by_cell

  # Finalize AR(1) calibration
  if (engine %in% c("ar1", "ar1_common", "ar1_anchored") && length(all_onestep) > 0) {
    onestep_dt <- data.table::rbindlist(all_onestep)

    if (noise_spec$cs_pool == "global") {
      # Pool across all cohorts
      innovation_pool <- onestep_dt$r_tilde[!is.na(onestep_dt$r_tilde)]
    } else {
      # Per-cohort pools (store as list keyed by cohort)
      calib$eta_pool_by_cohort <- onestep_dt[!is.na(r_tilde),
                                               .(pool = list(r_tilde)), by = cohort]
    }

    # Estimate rho from one-step innovations by unit
    # Create lagged innovations within each unit
    data.table::setorderv(onestep_dt, c("unit", "calendar_year"))
    onestep_dt[, r_tilde_lag := shift(r_tilde, 1L), by = unit]
    complete <- onestep_dt[!is.na(r_tilde_lag) & !is.na(r_tilde)]

    if (nrow(complete) > 5) {
      rho_hat <- stats::coef(stats::lm(r_tilde ~ 0 + r_tilde_lag, data = complete))[[1]]
      rho_hat <- max(0, min(rho_hat, 0.95))
    } else {
      rho_hat <- 0
    }
    if (!is.null(noise_spec$rho)) rho_hat <- noise_spec$rho
    calib$rho <- rho_hat

    # Compute AR(1) innovations (eta = r_tilde - rho * r_tilde_lag)
    complete[, eta := r_tilde - rho_hat * r_tilde_lag]
    # Always compute sigma_eta as fallback
    calib$sigma_eta <- stats::sd(complete$eta, na.rm = TRUE)
    if (is.na(calib$sigma_eta) || calib$sigma_eta == 0) {
      calib$sigma_eta <- stats::sd(onestep_dt$r_tilde, na.rm = TRUE)
    }

    if (noise_spec$cs_pool == "global") {
      if (noise_spec$innovation == "normal") {
        # sigma_eta already computed above
      } else {
        calib$eta_pool <- complete$eta[!is.na(complete$eta)]
      }

      # Fallback: if eta_pool is too small, use raw innovations
      if (noise_spec$innovation == "empirical" && length(calib$eta_pool) < 10) {
        calib$eta_pool <- innovation_pool
        calib$rho <- 0  # degenerate to iid
      }
    }

    # Common shock pool
    if ((engine %in% c("ar1_common", "ar1_anchored") || isTRUE(noise_spec$common_shock)) && length(all_common) > 0) {
      common_dt <- data.table::rbindlist(all_common)
      if (noise_spec$cs_pool == "global") {
        calib$u_pool <- common_dt$u_k[!is.na(common_dt$u_k)]
      } else {
        calib$u_pool_by_cohort <- common_dt[!is.na(u_k),
                                              .(pool = list(u_k)), by = cohort]
      }
    }
  }

  calib
}


# ---------------------------------------------------------------------------
# Drawing functions
# ---------------------------------------------------------------------------

#' Draw noise for imputation/Poisson PTA (level or log scale)
#'
#' @param calib Calibration object from calibrate_noise_imputation() or calibrate_noise_poisson()
#' @param units Vector of unit IDs for treated observations
#' @param times Vector of time values for treated observations
#' @param seed Optional random seed
#' @return data.table with columns: unit, time, eps
#' @export
draw_noise <- function(calib, units, times, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  n <- length(units)
  engine <- calib$engine

  if (engine == "none") {
    return(data.table::data.table(unit = units, time = times, eps = rep(0, n)))
  }

  if (engine == "iid") {
    sigma <- if (calib$scale == "additive") calib$sigma else calib$sigma_log
    eps <- stats::rnorm(n, mean = 0, sd = sigma)
    return(data.table::data.table(unit = units, time = times, eps = eps))
  }

  # AR(1) + ATT-weighted anchored common shocks (additive) / no common (log)
  if (engine == "ar1_anchored") {
    dt <- data.table::data.table(unit = units, time = times)
    data.table::setorderv(dt, c("unit", "time"))

    unique_units <- unique(dt$unit)
    unique_times <- sort(unique(dt$time))
    rho <- calib$rho

    # --- Common component (additive scale only) ---
    u_t <- rep(0, length(unique_times))
    names(u_t) <- as.character(unique_times)

    if (calib$scale == "additive" && !is.null(calib[["control_resid_by_year"]])) {
      # Draw one control residual per calendar year
      for (k in seq_along(unique_times)) {
        yr <- as.character(unique_times[k])
        pool <- calib[["control_resid_by_year"]][[yr]]
        if (!is.null(pool) && length(pool) > 0) {
          u_t[k] <- sample(pool, 1)
        }
      }

      # ATT-weighted centering: w_t = count(treated at t) / total
      time_counts <- table(factor(times, levels = unique_times))
      w_t <- as.numeric(time_counts) / sum(time_counts)
      weighted_mean <- sum(w_t * u_t)
      u_t <- u_t - weighted_mean
    }
    # Log scale: u_t stays zero (no common component for Poisson)

    # --- AR(1) idiosyncratic per unit ---
    eps_list <- list()
    for (uid in unique_units) {
      unit_times <- sort(dt[unit == uid, time])
      n_t <- length(unit_times)

      if (!is.null(calib[["eta_pool"]]) && length(calib[["eta_pool"]]) > 0) {
        eta <- sample(calib[["eta_pool"]], n_t, replace = TRUE)
      } else {
        sigma_eta <- if (!is.null(calib[["sigma_eta"]])) calib[["sigma_eta"]] else {
          if (calib[["scale"]] == "additive") calib[["sigma"]] else calib[["sigma_log"]]
        }
        eta <- stats::rnorm(n_t, 0, sigma_eta)
      }

      v <- numeric(n_t)
      v[1] <- eta[1]
      if (n_t > 1) {
        for (j in 2:n_t) {
          v[j] <- rho * v[j - 1] + eta[j]
        }
      }

      eps <- v + u_t[as.character(unit_times)]

      eps_list[[length(eps_list) + 1]] <- data.table::data.table(
        unit = rep(uid, n_t), time = unit_times, eps = eps
      )
    }

    result_dt <- data.table::rbindlist(eps_list)

    # Jensen correction for log scale
    if (calib$scale == "log" && isTRUE(calib[["jensen_correction"]])) {
      correction <- log(mean(exp(result_dt$eps)))
      result_dt[, eps := eps - correction]
    }

    return(result_dt)
  }

  # AR(1) or AR(1) + common shocks
  # Need to simulate per-unit time series
  dt <- data.table::data.table(unit = units, time = times)
  data.table::setorderv(dt, c("unit", "time"))

  unique_units <- unique(dt$unit)
  unique_times <- sort(unique(dt$time))
  rho <- calib$rho

  # Pre-draw common shocks if needed
  u_t <- rep(0, length(unique_times))
  names(u_t) <- as.character(unique_times)
  if (engine == "ar1_common" || isTRUE(calib[["common_shock"]])) {
    if (!is.null(calib[["u_pool"]]) && length(calib[["u_pool"]]) > 0) {
      # Draw common shock innovations and cumulate
      u_innovations <- sample(calib[["u_pool"]], length(unique_times), replace = TRUE)
      u_t <- cumsum(u_innovations)
      names(u_t) <- as.character(unique_times)
    }
  }

  # Simulate AR(1) per unit
  eps_list <- list()
  for (uid in unique_units) {
    unit_times <- sort(dt[unit == uid, time])
    n_t <- length(unit_times)

    # Draw innovations
    if (!is.null(calib[["eta_pool"]]) && length(calib[["eta_pool"]]) > 0) {
      eta <- sample(calib[["eta_pool"]], n_t, replace = TRUE)
    } else {
      sigma_eta <- if (!is.null(calib[["sigma_eta"]])) calib[["sigma_eta"]] else {
        if (calib[["scale"]] == "additive") calib[["sigma"]] else calib[["sigma_log"]]
      }
      eta <- stats::rnorm(n_t, 0, sigma_eta)
    }

    # AR(1) recursion
    v <- numeric(n_t)
    v[1] <- eta[1]
    if (n_t > 1) {
      for (j in 2:n_t) {
        v[j] <- rho * v[j - 1] + eta[j]
      }
    }

    # Add common shock
    eps <- v + u_t[as.character(unit_times)]

    eps_list[[length(eps_list) + 1]] <- data.table::data.table(
      unit = rep(uid, n_t),
      time = unit_times,
      eps = eps
    )
  }

  result_dt <- data.table::rbindlist(eps_list)

  # Jensen correction for log scale (ar1, ar1_common)
  if (calib$scale == "log" && isTRUE(calib[["jensen_correction"]])) {
    correction <- log(mean(exp(result_dt$eps)))
    result_dt[, eps := eps - correction]
  }

  result_dt
}


#' Draw noise for CS PTA (cumulated one-step shocks)
#'
#' For CS, noise at horizon rp is the cumulative sum of one-step shocks from
#' treatment onset through rp. This preserves the long-difference structure.
#'
#' @param calib Calibration object from calibrate_noise_cs()
#' @param treated_units_by_cohort Named list: cohort -> vector of treated unit IDs
#' @param max_rp_by_cohort Named list: cohort -> maximum relative period
#' @param seed Optional random seed
#' @return data.table with columns: unit, cohort, rp, eps_ld (cumulated shock)
#' @export
draw_noise_cs <- function(calib, treated_units_by_cohort, max_rp_by_cohort, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  engine <- calib$engine

  if (engine == "none") {
    # Return zeros for all
    result_list <- list()
    for (g in names(treated_units_by_cohort)) {
      units <- treated_units_by_cohort[[g]]
      max_rp <- max_rp_by_cohort[[g]]
      for (rp in 0:max_rp) {
        result_list[[length(result_list) + 1]] <- data.table::data.table(
          unit = units, cohort = as.numeric(g), rp = rp, eps_ld = 0
        )
      }
    }
    return(data.table::rbindlist(result_list))
  }

  if (engine == "iid") {
    # Per-cell independent draws (backward compat)
    result_list <- list()
    for (g in names(treated_units_by_cohort)) {
      units <- treated_units_by_cohort[[g]]
      max_rp <- max_rp_by_cohort[[g]]
      for (rp in 0:max_rp) {
        cell_key <- paste(g, rp, sep = "_")
        cell_sd <- calib$resid_sd_by_cell[[cell_key]]
        if (is.null(cell_sd) || is.na(cell_sd)) cell_sd <- 0
        eps <- stats::rnorm(length(units), mean = 0, sd = cell_sd)
        result_list[[length(result_list) + 1]] <- data.table::data.table(
          unit = units, cohort = as.numeric(g), rp = rp, eps_ld = eps
        )
      }
    }
    return(data.table::rbindlist(result_list))
  }

  # AR(1) or AR(1) + common shocks: simulate one-step shocks and cumulate
  rho <- if (!is.null(calib[["rho"]])) calib[["rho"]] else 0

  result_list <- list()

  for (g_char in names(treated_units_by_cohort)) {
    g <- as.numeric(g_char)
    units <- treated_units_by_cohort[[g_char]]
    max_rp <- max_rp_by_cohort[[g_char]]
    if (is.null(max_rp) || !is.finite(max_rp) || max_rp < 0) next
    n_units <- length(units)
    n_periods <- max_rp + 1  # rp = 0, 1, ..., max_rp

    # Get innovation pool/sigma for this cohort
    # NOTE: Use [[ ]] not $ to avoid R's partial name matching
    # (calib$eta_pool would partial-match to calib$eta_pool_by_cohort)
    eta_pool_local <- NULL
    if (!is.null(calib[["eta_pool"]])) {
      # Global pool
      eta_pool_local <- calib[["eta_pool"]]
    } else if (!is.null(calib[["eta_pool_by_cohort"]])) {
      # Per-cohort pool
      cohort_row <- calib[["eta_pool_by_cohort"]][cohort == g]
      if (nrow(cohort_row) > 0) {
        eta_pool_local <- cohort_row$pool[[1]]
      }
    }

    # Get common shock pool
    u_pool <- NULL
    if (engine %in% c("ar1_common", "ar1_anchored") || isTRUE(calib[["common_shock"]])) {
      if (!is.null(calib[["u_pool"]])) {
        u_pool <- calib[["u_pool"]]
      } else if (!is.null(calib[["u_pool_by_cohort"]])) {
        cohort_row <- calib[["u_pool_by_cohort"]][cohort == g]
        if (nrow(cohort_row) > 0) u_pool <- cohort_row$pool[[1]]
      }
    }

    # Draw common shocks for all periods in this cohort
    u_shocks <- rep(0, n_periods)
    if (!is.null(u_pool) && length(u_pool) > 0) {
      u_shocks <- sample(u_pool, n_periods, replace = TRUE)
    }

    # For each treated unit, simulate AR(1) idiosyncratic and cumulate
    for (i in seq_along(units)) {
      uid <- units[i]

      # Draw innovations
      if (!is.null(eta_pool_local) && length(eta_pool_local) > 0) {
        eta <- sample(eta_pool_local, n_periods, replace = TRUE)
      } else if (!is.null(calib[["sigma_eta"]])) {
        eta <- stats::rnorm(n_periods, 0, calib[["sigma_eta"]])
      } else {
        # Fallback: use per-cell resid_sd average
        avg_sd <- mean(unlist(calib[["resid_sd_by_cell"]]), na.rm = TRUE)
        eta <- stats::rnorm(n_periods, 0, avg_sd)
      }

      # AR(1) recursion for one-step idiosyncratic shocks
      v <- numeric(n_periods)
      v[1] <- eta[1]
      if (n_periods > 1) {
        for (j in 2:n_periods) {
          v[j] <- rho * v[j - 1] + eta[j]
        }
      }

      # One-step total shock = u_k + v_{i,k}
      one_step <- u_shocks + v

      # Cumulate to get long-diff shock at each rp
      eps_ld <- cumsum(one_step)

      result_list[[length(result_list) + 1]] <- data.table::data.table(
        unit = rep(uid, n_periods),
        cohort = rep(g, n_periods),
        rp = 0:max_rp,
        eps_ld = eps_ld
      )
    }
  }

  data.table::rbindlist(result_list)
}
