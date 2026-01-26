# adapter_etwfe_poisson_glm.R
# Saturated ETWFE Poisson (Wooldridge 2023) via fixest::fepois()
#
# Model: log(E[count]) = unit_FE + time_FE + X'beta + sum_k delta_k * D_{g_k,t_k} + log(pop)
# Each D_{g,t} = 1 for treated cohort g in year t (post-treatment, or all periods if event_study).
#
# ATT aggregation: We compute the ARITHMETIC MEAN of percent changes across cells:
#   ATT_pct = sum_k w_k * (exp(delta_k) - 1)
# where w_k = n_obs_k / sum(n_obs) are observation-proportional weights.
#
# This differs from the geometric mean approach (exp(sum w*delta) - 1) and is chosen for:
# 1. Comparability with linear estimators (CS, imputation) which average level effects
# 2. Policy interpretability: "average percent change across treated units"
#
# SE via delta method: grad = w * exp(delta), SE = sqrt(grad' * Sigma * grad)
# Event study: aggregate by relative time e = t - g, with e=-1 as reference period.

#' ETWFE Poisson Adapter (Saturated Wooldridge 2023)
#'
#' Uses fixest::fepois() with saturated cohort x time treatment indicators
#' and observation-proportional aggregation weights for the overall ATT.
#'
#' @return An estimator_adapter registered as "etwfe_poisson"
#' @export
adapter_etwfe_poisson_glm <- function() {

  # FIT FUNCTION
  fit_fn <- function(data,
                     outcome_var,
                     time_var,
                     id_var,
                     group_var,
                     controls = NULL,
                     cluster_var = NULL,
                     n_cores = 1,
                     event_study = FALSE,
                     weightsname = NULL,
                     outcome_type = NULL,
                     pop_var = NULL,
                     family = "poisson",
                     pretrend_test = FALSE,
                     base_period = "varying",
                     allow_unbalanced_panel = FALSE,
                     trend_type = "common",
                     trend_order = 1L,
                     ...) {

    if (!requireNamespace("fixest", quietly = TRUE)) {
      stop("Package 'fixest' is required for ETWFE Poisson. Install with: install.packages('fixest')")
    }

    # Convert to data.table
    dt <- data.table::copy(data.table::as.data.table(data))

    # Default cluster to id_var
    if (is.null(cluster_var)) {
      cluster_var <- id_var
    }

    # --- Handle outcome_type: determine count variable and offset ---
    if (!is.null(outcome_type) && outcome_type == "count") {
      # outcome_var IS the count; pop_var is the population for offset
      if (is.null(pop_var)) {
        stop("ETWFE Poisson with outcome_type='count' requires pop_var for offset")
      }
      dt[, .count := as.numeric(get(outcome_var))]
      dt[, .log_pop := log(as.numeric(get(pop_var)))]
    } else {
      # outcome_var is a rate: convert to count = rate * pop / 100000
      # Note: We do NOT round to integer. Poisson QMLE works fine with non-integer
      # "counts" and rounding introduces systematic bias for rare events.
      if (is.null(pop_var)) {
        stop("ETWFE Poisson with rate outcome requires pop_var to convert to count")
      }
      dt[, .count := as.numeric(get(outcome_var)) * as.numeric(get(pop_var)) / 1e5]
      dt[, .log_pop := log(as.numeric(get(pop_var)))]
    }

    # Drop rows with non-positive population or NA
    dt <- dt[is.finite(.log_pop) & .count >= 0 & !is.na(.count)]

    # Drop rows with NA controls
    if (!is.null(controls)) {
      for (ctrl in controls) {
        if (ctrl %in% names(dt)) {
          dt <- dt[!is.na(get(ctrl))]
        }
      }
    }

    if (nrow(dt) < 50) {
      stop("Insufficient data after filtering (< 50 rows)")
    }

    # --- Create saturated cohort x time treatment indicators ---
    cohort_vals <- dt[[group_var]]
    time_vals <- dt[[time_var]]

    # Validate that time and cohort are integer-valued (required for e = t - g arithmetic)
    if (!is.numeric(time_vals) || !all(time_vals == as.integer(time_vals), na.rm = TRUE)) {
      stop("time_var must be integer-valued (e.g., years as integers, not Date or numeric with decimals)")
    }
    if (!is.numeric(cohort_vals) || !all(cohort_vals == as.integer(cohort_vals), na.rm = TRUE)) {
      stop("group_var must be integer-valued (e.g., treatment year as integer)")
    }
    # Coerce to integer for safe arithmetic
    time_vals <- as.integer(time_vals)
    cohort_vals <- as.integer(cohort_vals)
    dt[, (time_var) := as.integer(get(time_var))]
    dt[, (group_var) := as.integer(get(group_var))]

    if (event_study) {
      # Event study mode: include ALL periods of treated cohorts (leads + lags)
      # Reference: e = -1 (year before treatment) → part of "ref"
      # This tests multiplicative PT (pre-treatment should be ~0)
      dt[, gt_cell := ifelse(
        !is.na(cohort_vals) & cohort_vals > 0 & time_vals != (cohort_vals - 1L),
        paste0("g", cohort_vals, "_t", time_vals),
        "ref"
      )]
    } else {
      # Standard mode: only post-treatment D_{g,t} (more efficient for ATT only)
      dt[, gt_cell := ifelse(
        !is.na(cohort_vals) & cohort_vals > 0 & time_vals >= cohort_vals,
        paste0("g", cohort_vals, "_t", time_vals),
        "ref"
      )]
    }
    dt[, gt_cell := factor(gt_cell)]
    dt[, gt_cell := stats::relevel(gt_cell, ref = "ref")]

    # Check we have treatment cells
    n_treat_cells <- length(levels(dt$gt_cell)) - 1
    if (n_treat_cells < 1) {
      stop("No treatment cells found (no post-treatment observations)")
    }

    # --- Build formula for fixest ---
    rhs_main <- "gt_cell"

    # Add control variables
    if (!is.null(controls)) {
      ctrl_exist <- controls[controls %in% names(dt)]
      if (length(ctrl_exist) > 0) {
        rhs_main <- paste0(rhs_main, " + ", paste(ctrl_exist, collapse = " + "))
      }
    }

    # Build fixed effects part - with or without unit-specific trends
    if (trend_type == "unit_trend") {
      # EXPERIMENTAL: Unit-specific linear trends
      # WARNING: This may cause numerical issues (near-singular matrices) with many units.
      warning("trend_type='unit_trend' is experimental and may cause numerical issues with many units")
      # id_var[time_var] estimates unit FE + unit-specific slope on time
      rhs_fe <- paste0(id_var, "[", time_var, "] + ", time_var)
    } else {
      # Common trends (default): just unit and time FEs
      rhs_fe <- paste0(id_var, " + ", time_var)
    }
    fml <- stats::as.formula(paste0(".count ~ ", rhs_main, " | ", rhs_fe))

    # --- Fit saturated Poisson ETWFE ---
    fit <- fixest::fepois(fml, data = dt, offset = ~.log_pop,
                          vcov = stats::as.formula(paste0("~", cluster_var)),
                          notes = FALSE, warn = FALSE)

    # --- Extract delta_{g,t} coefficients and clustered VCV ---
    all_coefs <- stats::coef(fit)
    delta_idx <- grep("^gt_cell", names(all_coefs))

    if (length(delta_idx) == 0) {
      stop("No gt_cell coefficients found in model output")
    }

    delta_coefs <- all_coefs[delta_idx]
    full_vcov <- stats::vcov(fit)
    delta_vcov <- full_vcov[delta_idx, delta_idx, drop = FALSE]

    # Remove NA coefficients (from collinearity / empty cells)
    valid <- !is.na(delta_coefs)
    if (sum(valid) == 0) {
      stop("All gt_cell coefficients are NA (perfect collinearity)")
    }
    delta_coefs <- delta_coefs[valid]
    delta_vcov <- delta_vcov[valid, valid, drop = FALSE]
    delta_names <- names(all_coefs)[delta_idx][valid]

    # --- Parse cell names to get relative times ---
    # Handle potential fixest naming variations (gt_cell, gt_cell::, etc.)
    cell_levels <- sub("^gt_cell[:]*", "", delta_names)
    cell_levels <- sub("^:+", "", cell_levels)
    cell_g <- as.integer(gsub("g(\\d+)_t-?\\d+", "\\1", cell_levels))
    cell_t <- as.integer(gsub("g\\d+_t(-?\\d+)", "\\1", cell_levels))
    rel_times <- cell_t - cell_g

    # --- Compute observation counts per cell ---
    n_per_cell <- numeric(length(cell_levels))
    for (k in seq_along(cell_levels)) {
      n_per_cell[k] <- dt[gt_cell == cell_levels[k], .N]
    }

    # --- Overall ATT: aggregate POST-TREATMENT cells only (e >= 0) ---
    post_idx <- which(rel_times >= 0)
    if (length(post_idx) == 0) {
      stop("No post-treatment cells found for ATT aggregation")
    }

    post_weights <- n_per_cell[post_idx] / sum(n_per_cell[post_idx])

    # --- ATT on log scale (weighted average of log effects) ---
    # This is the "geometric mean" approach - kept for reference/diagnostics
    att_log <- sum(post_weights * delta_coefs[post_idx])
    se_log <- sqrt(as.numeric(
      t(post_weights) %*% delta_vcov[post_idx, post_idx, drop = FALSE] %*% post_weights
    ))

    # --- ATT as ARITHMETIC MEAN of percent changes ---
    # This is the primary estimand: ATT_pct = sum_k w_k * (exp(delta_k) - 1)
    # This is comparable to linear estimators' percent change = ATT_level / baseline
    post_deltas <- delta_coefs[post_idx]
    post_vcov <- delta_vcov[post_idx, post_idx, drop = FALSE]

    # Arithmetic mean of percent changes
    att_pct <- sum(post_weights * (exp(post_deltas) - 1))

    # Delta method SE: gradient of sum(w * (exp(delta) - 1)) w.r.t. delta is w * exp(delta)
    grad_pct <- post_weights * exp(post_deltas)
    se_pct <- sqrt(as.numeric(t(grad_pct) %*% post_vcov %*% grad_pct))

    # --- Event study: aggregate by relative time ---
    # Returns both log-scale ATT and percent change for each relative time
    event_study_result <- NULL
    if (event_study) {
      unique_e <- sort(unique(rel_times))
      es_att_log <- numeric(length(unique_e))
      es_se_log <- numeric(length(unique_e))
      es_att_pct <- numeric(length(unique_e))
      es_se_pct <- numeric(length(unique_e))

      for (i in seq_along(unique_e)) {
        e <- unique_e[i]
        idx <- which(rel_times == e)
        w_e <- n_per_cell[idx] / sum(n_per_cell[idx])
        deltas_e <- delta_coefs[idx]
        vcov_e <- delta_vcov[idx, idx, drop = FALSE]

        # Log-scale (geometric mean)
        es_att_log[i] <- sum(w_e * deltas_e)
        es_se_log[i] <- sqrt(as.numeric(t(w_e) %*% vcov_e %*% w_e))

        # Percent change (arithmetic mean) - consistent with overall ATT
        es_att_pct[i] <- sum(w_e * (exp(deltas_e) - 1))
        grad_e <- w_e * exp(deltas_e)
        es_se_pct[i] <- sqrt(as.numeric(t(grad_e) %*% vcov_e %*% grad_e))
      }

      event_study_result <- data.table::data.table(
        rel_time = unique_e,
        att = es_att_log,        # Log-scale for backward compatibility
        se = es_se_log,
        att_pct = es_att_pct,    # Percent change (arithmetic mean)
        se_pct = es_se_pct
      )

      # Add the omitted reference period (e = -1) with zeros
      # This makes plotting and pre-trend checks cleaner
      if (!(-1L %in% unique_e)) {
        ref_row <- data.table::data.table(
          rel_time = -1L,
          att = 0,
          se = 0,
          att_pct = 0,
          se_pct = 0
        )
        event_study_result <- data.table::rbindlist(list(ref_row, event_study_result))
        event_study_result <- event_study_result[order(rel_time)]
      }
    }

    # Mean outcome in control group (for interpretability)
    # Returns rate per 100k (same scale as input rate data)
    mean_y <- mean(dt[gt_cell == "ref", .count / exp(.log_pop) * 1e5], na.rm = TRUE)

    # --- Level-scale effect (comparable to etwfe package's emfx output) ---
    # ATT in level units = mean_y_control * att_pct
    # This represents the change in rate per 100k (or count if outcome_type='count')
    att_level <- mean_y * att_pct
    # SE via delta method: d(mean_y * att_pct)/d(delta) = mean_y * d(att_pct)/d(delta)
    #                                                    = mean_y * w * exp(delta)
    se_level <- mean_y * se_pct

    # Build metadata
    metadata <- list(
      # Primary estimand: arithmetic mean of percent changes (comparable to linear estimators)
      att_pct = att_pct,
      att_se_pct = se_pct,
      # Level-scale effect (comparable to etwfe package's emfx output)
      att_level = att_level,        # Change in rate per 100k
      att_se_level = se_level,
      # Secondary: geometric mean (log-scale average) for reference
      att_log_geometric = att_log,
      att_se_log = se_log,
      att_pct_geometric = exp(att_log) - 1,  # For comparison
      # Sample info
      n_obs = nrow(dt),
      n_clusters = data.table::uniqueN(dt[[cluster_var]]),
      n_treat_cells = length(post_idx),
      n_lead_cells = if (event_study) sum(rel_times < 0) else 0L,
      cluster_var = cluster_var,
      mean_y_control = mean_y,  # Rate per 100k
      outcome_type_input = ifelse(is.null(outcome_type), "rate (assumed)", outcome_type),
      se_type = "clustered_fixest",
      trend_type = trend_type,
      trend_order = if (trend_type == "unit_trend") 1L else NA_integer_,
      aggregation_method = "arithmetic_mean_pct",
      weight_type = "observation_proportional"
    )

    # Return result
    list(
      agg = list(att = att_log, se = se_log),
      event_study = event_study_result,
      raw = fit,
      metadata = metadata
    )
  }

  # EXTRACT FUNCTION: Convert to standard_estimate
  extract_fn <- function(result) {
    standard_estimate(
      att = result$agg$att,
      se = result$agg$se,
      model_name = "etwfe_poisson",
      event_study = result$event_study,
      raw_result = result$raw,
      metadata = result$metadata
    )
  }

  estimator_adapter(
    name = "etwfe_poisson",
    fit_fn = fit_fn,
    extract_fn = extract_fn,
    requires = "fixest"
  )
}
