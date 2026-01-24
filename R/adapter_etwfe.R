# R/adapter_etwfe.R
#' Extended TWFE (Wooldridge) Adapter
#'
#' Translates between staggeredpower's common interface and the `etwfe` package.
#' Supports both linear and Poisson (for rare events/counts) estimation.
#'
#' @details
#' The etwfe package implements Wooldridge's Extended TWFE approach, which:
#' - Linear mode (family = NULL): Equivalent to CS/did2s, additive PT assumption
#' - Poisson mode (family = "poisson"): Multiplicative PT assumption on log-rate
#'
#' For Poisson mode with count data:
#' - Uses count as outcome with log(population) as offset
#' - Coefficients are on log-rate scale
#' - ATT is back-transformed to rate scale for comparability
#'
#' @return An estimator_adapter for ETWFE estimation
#' @export
#'
#' @examples
#' \dontrun{
#' adapter <- adapter_etwfe()
#' result <- adapter$fit(data = my_data, outcome_var = "y", family = "poisson", ...)
#' std_result <- adapter$extract(result)
#' }
adapter_etwfe <- function() {

  # Constants for rate scaling (per 100,000 population)
  RATE_SCALE <- 100000

  # FIT FUNCTION: Common params → etwfe::etwfe() + etwfe::emfx() call
  fit_fn <- function(data,
                     outcome_var,
                     time_var,
                     id_var,
                     group_var,
                     controls = NULL,
                     cluster_var = NULL,
                     n_cores = NULL,
                     event_study = FALSE,
                     weightsname = NULL,
                     family = NULL,
                     outcome_type = NULL,
                     pop_var = NULL,
                     pretrend_test = FALSE,
                     ...) {

    # Convert to data.table for manipulation (make a copy to avoid modifying original)
    data <- data.table::copy(data.table::as.data.table(data))

    # Track temporary columns for cleanup
    temp_cols <- character(0)

    # Default cluster to id_var
    if (is.null(cluster_var)) {
      cluster_var <- id_var
    }

    # Save original data before any modifications if pretrend testing is needed
    # (vanilla Poisson ES and CV comparison expect NA for never-treated)
    original_data <- if (pretrend_test) {
      data.table::copy(data)
    } else {
      NULL
    }

    # etwfe requires never-treated units to be coded as 0, not NA
    # Convert NA in group_var to 0
    if (any(is.na(data[[group_var]]))) {
      data[is.na(get(group_var)), (group_var) := 0]
    }

    # Validate population variable if provided
    if (!is.null(pop_var)) {
      if (!(pop_var %in% names(data))) {
        stop(sprintf("pop_var '%s' not found in data", pop_var))
      }
      pop_values <- data[[pop_var]]
      if (any(is.na(pop_values))) {
        warning("pop_var contains NA values; these observations may cause issues")
      }
      if (any(pop_values <= 0, na.rm = TRUE)) {
        stop("pop_var contains non-positive values; cannot compute log(population)")
      }
    }

    # Determine working outcome and offset based on family and outcome_type
    working_outcome <- outcome_var
    offset_var <- NULL
    transformation_applied <- "none"
    outcome_scale <- "rate"

    if (!is.null(family) && family == "poisson") {
      # Poisson mode: need counts + offset

      if (is.null(outcome_type) || outcome_type == "rate") {
        # Input is rate, need to convert to count
        if (is.null(pop_var)) {
          stop("etwfe Poisson mode with rate data requires pop_var to compute counts")
        }

        # Create count variable: count = rate * pop / RATE_SCALE
        data[, .etwfe_count := get(outcome_var) * get(pop_var) / RATE_SCALE]
        data[, .etwfe_log_pop := log(get(pop_var))]
        temp_cols <- c(temp_cols, ".etwfe_count", ".etwfe_log_pop")
        working_outcome <- ".etwfe_count"
        offset_var <- ".etwfe_log_pop"
        transformation_applied <- "rate_to_count"
        outcome_scale <- "count"

      } else if (outcome_type == "count") {
        # Input is count, use directly with log(pop) offset
        if (is.null(pop_var)) {
          stop("etwfe Poisson mode with count data requires pop_var for offset")
        }

        data[, .etwfe_log_pop := log(get(pop_var))]
        temp_cols <- c(temp_cols, ".etwfe_log_pop")
        offset_var <- ".etwfe_log_pop"
        transformation_applied <- "none"
        outcome_scale <- "count"
      }

    } else {
      # Linear mode: work with rates

      if (!is.null(outcome_type) && outcome_type == "count") {
        # Input is count, convert to rate for linear model
        if (is.null(pop_var)) {
          stop("etwfe linear mode with count data requires pop_var to compute rates")
        }

        data[, .etwfe_rate := get(outcome_var) / get(pop_var) * RATE_SCALE]
        temp_cols <- c(temp_cols, ".etwfe_rate")
        working_outcome <- ".etwfe_rate"
        transformation_applied <- "count_to_rate"
        outcome_scale <- "rate"

      } else {
        # Input is rate, use directly
        transformation_applied <- "none"
        outcome_scale <- "rate"
      }
    }

    # Build formula
    if (!is.null(controls) && length(controls) > 0) {
      fml <- stats::as.formula(paste0(working_outcome, " ~ ", paste(controls, collapse = " + ")))
    } else {
      fml <- stats::as.formula(paste0(working_outcome, " ~ 1"))
    }

    # Build vcov specification for clustering
    vcov_spec <- stats::as.formula(paste0("~", cluster_var))

    # Store mean of treated outcomes for back-transformation
    # Need to identify treated observations
    data[, .treat_indicator := ifelse(!is.na(get(group_var)) & get(time_var) >= get(group_var), 1, 0)]
    temp_cols <- c(temp_cols, ".treat_indicator")

    # Validate that we have treated observations
    n_treated <- sum(data$.treat_indicator == 1, na.rm = TRUE)
    if (n_treated == 0) {
      stop("No treated observations found. Check that group_var and time_var are correctly specified.")
    }

    mean_treated_rate <- if (outcome_scale == "count" && !is.null(pop_var)) {
      # Compute rate for treated observations
      mean(data[.treat_indicator == 1, get(outcome_var) / get(pop_var) * RATE_SCALE], na.rm = TRUE)
    } else if (transformation_applied == "rate_to_count") {
      # Original was rate
      mean(data[.treat_indicator == 1, get(outcome_var)], na.rm = TRUE)
    } else {
      mean(data[.treat_indicator == 1, get(working_outcome)], na.rm = TRUE)
    }

    # Call etwfe::etwfe
    etwfe_args <- list(
      fml = fml,
      tvar = time_var,
      gvar = group_var,
      data = as.data.frame(data),
      vcov = vcov_spec
    )

    # Add family if specified
    if (!is.null(family)) {
      etwfe_args$family <- family
    }

    # Note: etwfe doesn't support offset directly
    # For Poisson on counts, results are scaled back to rate in post-processing

    # Run etwfe with error handling
    m_etwfe <- tryCatch({
      do.call(etwfe::etwfe, etwfe_args)
    }, error = function(e) {
      # Clean up temporary columns before rethrowing
      if (length(temp_cols) > 0) {
        data[, (temp_cols) := NULL]
      }
      stop(sprintf("etwfe::etwfe() failed: %s", e$message))
    })

    # Get aggregated ATT using emfx with error handling
    m_etwfe_agg <- tryCatch({
      etwfe::emfx(m_etwfe, type = "simple")
    }, error = function(e) {
      # Clean up temporary columns before rethrowing
      if (length(temp_cols) > 0) {
        data[, (temp_cols) := NULL]
      }
      stop(sprintf("etwfe::emfx() failed: %s", e$message))
    })

    # Extract ATT and SE
    # emfx returns a data.frame with estimate, std.error, etc.
    att_raw <- m_etwfe_agg$estimate[1]
    se_raw <- m_etwfe_agg$std.error[1]

    # Back-transform if Poisson (results are on log-rate scale)
    # Also compute IRR (incidence rate ratio)
    irr <- NA
    irr_ci_low <- NA
    irr_ci_high <- NA

    if (!is.null(family) && family == "poisson") {
      # emfx returns average marginal effects on the count scale
      # ATT_count = E[Y(1)] - E[Y(0)] = E[Y(0)] * (IRR - 1)
      #
      # To get IRR: IRR = 1 + ATT / E[Y(0)]
      # We approximate E[Y(0)] ≈ mean(Y_treated) - ATT

      # If outcome was count, convert ATT to rate scale
      if (outcome_scale == "count") {
        mean_pop <- mean(data[[pop_var]], na.rm = TRUE)
        if (mean_pop <= 0) {
          warning("Mean population is non-positive; cannot scale ATT to rate. Using raw values.")
          att_rate_scale <- att_raw
          se_rate_scale <- se_raw
        } else {
          att_rate_scale <- att_raw / mean_pop * RATE_SCALE
          se_rate_scale <- se_raw / mean_pop * RATE_SCALE
        }

        # Compute IRR from count-scale marginal effect
        # E[Y(0)] ≈ mean treated count - ATT
        mean_treated_count <- mean(data[.treat_indicator == 1, get(working_outcome)], na.rm = TRUE)
        e_y0_count <- mean_treated_count - att_raw

        if (is.na(e_y0_count) || e_y0_count <= 0) {
          warning("E[Y(0)] is non-positive or NA; cannot compute IRR. Setting IRR to NA.")
          irr <- NA
          irr_ci_low <- NA
          irr_ci_high <- NA
        } else {
          irr <- 1 + att_raw / e_y0_count
          # Delta method for IRR SE: SE(IRR) ≈ SE(ATT) / E[Y(0)]
          irr_se <- se_raw / e_y0_count
          irr_ci_low <- irr - 1.96 * irr_se
          irr_ci_high <- irr + 1.96 * irr_se
        }
      } else {
        att_rate_scale <- att_raw
        se_rate_scale <- se_raw
      }

    } else {
      # Linear model - results already on appropriate scale
      att_rate_scale <- att_raw
      se_rate_scale <- se_raw
    }

    # Event study if requested
    event_study_result <- NULL
    if (event_study) {
      event_study_result <- tryCatch({
        m_etwfe_event <- etwfe::emfx(m_etwfe, type = "event")

        # Transform if needed (same logic as above)
        if (!is.null(family) && family == "poisson" && outcome_scale == "count") {
          mean_pop <- mean(data[[pop_var]], na.rm = TRUE)
          if (mean_pop > 0) {
            data.table::data.table(
              rel_time = m_etwfe_event$event,
              att = m_etwfe_event$estimate / mean_pop * RATE_SCALE,
              se = m_etwfe_event$std.error / mean_pop * RATE_SCALE
            )
          } else {
            warning("Mean population is non-positive; event study ATTs not scaled to rate.")
            data.table::data.table(
              rel_time = m_etwfe_event$event,
              att = m_etwfe_event$estimate,
              se = m_etwfe_event$std.error
            )
          }
        } else {
          data.table::data.table(
            rel_time = m_etwfe_event$event,
            att = m_etwfe_event$estimate,
            se = m_etwfe_event$std.error
          )
        }
      }, error = function(e) {
        warning(sprintf("Event study failed for etwfe: %s", e$message))
        NULL
      })
    }

    # Compute mean_pop_for_scaling before cleanup
    mean_pop_for_scaling <- if (!is.null(family) && family == "poisson" && outcome_scale == "count") {
      mean(data[[pop_var]], na.rm = TRUE)
    } else {
      NA
    }

    # Pre-trend test for Poisson models
    pt_result <- NULL
    if (pretrend_test) {
      if (!is.null(family) && family == "poisson") {
        message("Note: ETWFE does not produce pre-treatment coefficients by design.\n",
                "Running supplementary vanilla Poisson event study for pre-trend test.")

        # Run vanilla Poisson event study
        # Use original_data which has NA for never-treated units
        vanilla_result <- tryCatch({
          run_vanilla_poisson_es(
            data = original_data,
            outcome_var = working_outcome,
            time_var = time_var,
            id_var = id_var,
            group_var = group_var,
            cluster_var = cluster_var
          )
        }, error = function(e) {
          list(
            pre_coefs = numeric(0),
            pre_vcov = matrix(nrow = 0, ncol = 0),
            method = "vanilla_poisson_event_study"
          )
        })

        # Compute Wald test
        pt_result <- compute_pretrend_wald_test(
          vanilla_result$pre_coefs,
          vanilla_result$pre_vcov
        )
        pt_result$method <- "vanilla_poisson_event_study"

        # Compute CV comparison for Poisson
        # Use original_data (NA for never-treated) and working_outcome (count variable)
        cv_result <- tryCatch({
          compute_cv_comparison(
            data = original_data,
            outcome_var = working_outcome,
            time_var = time_var,
            group_var = group_var,
            id_var = id_var
          )
        }, error = function(e) {
          list(ratio_cv = NA_real_, diff_cv = NA_real_, recommendation = NA_character_)
        })
        pt_result$cv_comparison <- cv_result

      } else {
        # Linear ETWFE - warn about limitation
        pt_result <- list(
          p_value = NA_real_,
          wald_stat = NA_real_,
          df = NA_integer_,
          reject_at_05 = NA,
          warning = "ETWFE does not produce pre-treatment coefficients. Use event_study with CS or did2s for additive PT testing.",
          method = "not_available"
        )
      }
    }

    # Clean up temporary columns (data is a copy, but good practice)
    if (length(temp_cols) > 0) {
      data[, (temp_cols) := NULL]
    }

    # Return results with metadata
    metadata <- list(
      family = ifelse(is.null(family), "gaussian", family),
      outcome_type_input = ifelse(is.null(outcome_type), "rate (assumed)", outcome_type),
      transformation_applied = transformation_applied,
      outcome_scale_used = outcome_scale,
      pt_assumption = ifelse(!is.null(family) && family == "poisson",
                             "multiplicative (log-rate scale)",
                             "additive (level scale)"),
      offset_used = FALSE,  # etwfe doesn't support offset; scaling done in post-processing
      mean_treated_rate = mean_treated_rate,
      mean_pop_for_scaling = mean_pop_for_scaling,
      # IRR interpretation for Poisson
      irr = irr,
      irr_interpretation = if (!is.na(irr)) {
        pct_change <- round((irr - 1) * 100, 1)
        if (pct_change < 0) {
          sprintf("Treatment reduces rate by %.1f%%", abs(pct_change))
        } else {
          sprintf("Treatment increases rate by %.1f%%", pct_change)
        }
      } else {
        NA
      }
    )

    # Add pretrend_test to metadata if computed
    if (!is.null(pt_result)) {
      metadata$pretrend_test <- pt_result
    }

    list(
      att = att_rate_scale,
      se = se_rate_scale,
      att_raw = att_raw,
      se_raw = se_raw,
      event_study = event_study_result,
      raw = m_etwfe,
      # IRR (incidence rate ratio) for Poisson models
      irr = irr,
      irr_ci_low = irr_ci_low,
      irr_ci_high = irr_ci_high,
      metadata = metadata
    )
  }

  # EXTRACT FUNCTION: etwfe result → standard_estimate
  extract_fn <- function(result) {
    standard_estimate(
      att = result$att,
      se = result$se,
      model_name = "etwfe",
      event_study = result$event_study,
      raw_result = result$raw,
      metadata = result$metadata
    )
  }

  estimator_adapter(
    name = "etwfe",
    fit_fn = fit_fn,
    extract_fn = extract_fn,
    requires = "etwfe"
  )
}
