# R/adapter_imputation.R
#' Borusyak-Jaravel-Spiess Imputation Adapter
#'
#' Translates between staggeredpower's common interface and the `didimputation` package.
#' Assumes additive parallel trends (level scale).
#'
#' @return An estimator_adapter for imputation-based estimation
#' @export
#'
#' @examples
#' \donttest{
#' adapter <- adapter_imputation()
#' # result <- adapter$fit(data = my_data, outcome_var = "y",
#' #   time_var = "year", id_var = "unit", group_var = "group")
#' # std_result <- adapter$extract(result)
#' }
adapter_imputation <- function() {

  # FIT FUNCTION: Common params → didimputation::did_imputation() call
  fit_fn <- function(data,
                     outcome_var,
                     time_var,
                     id_var,
                     group_var,
                     controls = NULL,
                     cluster_var = NULL,
                     event_study = FALSE,
                     weightsname = NULL,
                     outcome_type = NULL,
                     pop_var = NULL,
                     family = NULL,  # ignored by imputation, but accepted for compatibility
                     trend_type = "common",
                     trend_order = 1L,
                     ...) {

    if (!requireNamespace("didimputation", quietly = TRUE)) {
      stop("Package 'didimputation' is required for the imputation adapter. ",
           "Install with: install.packages('didimputation')", call. = FALSE)
    }

    # Convert to data.table for manipulation
    data <- data.table::as.data.table(data)

    # didimputation requires never-treated units to be coded as 0, not NA
    if (any(is.na(data[[group_var]]))) {
      data[is.na(get(group_var)), (group_var) := 0L]
    }

    # Default cluster to id_var
    if (is.null(cluster_var)) {
      cluster_var <- id_var
    }

    # Handle count → rate transformation if needed
    working_outcome <- outcome_var
    transformation_applied <- "none"

    if (!is.null(outcome_type) && outcome_type == "count") {
      if (is.null(pop_var)) {
        stop("imputation adapter with outcome_type='count' requires pop_var to compute rate")
      }
      # Create rate variable: rate = count / pop * 100000
      data[, .imputation_rate := get(outcome_var) / get(pop_var) * 100000]
      working_outcome <- ".imputation_rate"
      transformation_applied <- "count_to_rate"
    }

    # Build first_stage formula based on trend_type
    first_stage_formula <- NULL

    if (trend_type == "unit_trend") {
      # EXPERIMENTAL: Unit-specific linear trends using fixest varying slopes notation
      # WARNING: This may cause numerical issues (near-singular matrices) with many units.
      # Formula: ~ 0 | id_var[time_var] estimates unit FE + unit-specific slope on time
      # Add controls if present
      warning("trend_type='unit_trend' is experimental and may cause numerical issues with many units")
      if (!is.null(controls) && length(controls) > 0) {
        first_stage_formula <- stats::as.formula(
          paste0("~ ", paste(controls, collapse = " + "), " | ", id_var, "[", time_var, "]")
        )
      } else {
        first_stage_formula <- stats::as.formula(
          paste0("~ 0 | ", id_var, "[", time_var, "]")
        )
      }
    } else {
      # Common trends (default): no special first_stage, just controls if present
      if (!is.null(controls) && length(controls) > 0) {
        first_stage_formula <- stats::as.formula(
          paste0("~ 0 + ", paste(controls, collapse = " + "))
        )
      }
    }

    # Common args for did_imputation
    imp_args <- list(
      data = as.data.frame(data),
      yname = working_outcome,
      gname = group_var,
      tname = time_var,
      idname = id_var,
      first_stage = first_stage_formula,
      wname = weightsname,
      cluster_var = cluster_var
    )

    # Always get overall ATT (horizon = NULL → single-row result)
    result_att <- do.call(didimputation::did_imputation,
                          c(imp_args, list(horizon = NULL, pretrends = NULL)))

    # Optional state-cluster nonparametric bootstrap SE (small-N inference: the didimputation
    # analytic cluster-robust SE is ~anti-conservative in few-treated/few-cluster panels). Each
    # rep resamples clusters and re-runs did_imputation, so the untreated-surface estimation
    # uncertainty is carried through. Gated OFF by default via option 'staggeredpower.imp_boot'
    # (= number of bootstrap reps; 0 = analytic SE, unchanged behavior).
    .imp_boot_se <- NA_real_
    .imp_boot_B  <- getOption("staggeredpower.imp_boot", 0L)
    if (is.numeric(.imp_boot_B) && .imp_boot_B > 0) {
      .dtb  <- data.table::as.data.table(data)
      .cids <- unique(.dtb[[cluster_var]])
      .ests <- numeric(.imp_boot_B)
      for (.b in seq_len(.imp_boot_B)) {
        .samp  <- sample(.cids, length(.cids), replace = TRUE)
        # Relabel each resampled cluster with a small, distinct sequential id. NOTE: use small
        # ids (i), NOT i * 100000L -- large integer ids destabilize didimputation's first stage
        # (LU/Cholesky failure): empirically 0/30 resamples converge at i*1e5 vs 30/30 at i*1e3 or
        # sequential i. The old i*100000L silently returned NA on every rep, so .imp_boot_se stayed
        # NA and the code fell back to the analytic SE (the bootstrap never actually ran).
        .parts <- lapply(seq_along(.samp), function(i) {
          xx <- data.table::copy(.dtb[get(cluster_var) == .samp[i]]); xx[[id_var]] <- i; xx
        })
        .bd <- data.table::rbindlist(.parts)
        .mb <- tryCatch(do.call(didimputation::did_imputation,
                 c(list(data = as.data.frame(.bd), yname = working_outcome, gname = group_var,
                        tname = time_var, idname = id_var, first_stage = first_stage_formula,
                        wname = weightsname, cluster_var = id_var),
                   list(horizon = NULL, pretrends = NULL))), error = function(e) NULL)
        .ests[.b] <- if (is.null(.mb)) NA_real_ else .mb$estimate[1]
      }
      .ests <- .ests[is.finite(.ests)]
      if (length(.ests) >= 10) .imp_boot_se <- stats::sd(.ests)
    }
    .se_out <- if (is.finite(.imp_boot_se)) .imp_boot_se else result_att$std.error[1]

    # Event study: second call with horizon = TRUE
    event_study_result <- NULL
    if (event_study) {
      es_raw <- tryCatch({
        do.call(didimputation::did_imputation,
                c(imp_args, list(horizon = TRUE, pretrends = TRUE)))
      }, error = function(e) {
        warning(sprintf("Imputation event study failed: %s", e$message))
        NULL
      })

      if (!is.null(es_raw) && nrow(es_raw) > 0) {
        # Parse term column: could be "horizon::3" or just "3"
        if (grepl("::", es_raw$term[1])) {
          rel_times <- as.integer(gsub(".*::(-?[0-9]+)$", "\\1", es_raw$term))
        } else {
          rel_times <- as.integer(es_raw$term)
        }
        event_study_result <- data.table::data.table(
          rel_time = rel_times,
          att = es_raw$estimate,
          se = es_raw$std.error
        )
        # Sort by relative time
        event_study_result <- event_study_result[order(rel_time)]
      }
    }

    list(
      estimate = result_att$estimate[1],
      std.error = .se_out,
      event_study = event_study_result,
      raw = result_att,
      metadata = list(
        method = "imputation",
        package = "didimputation",
        pt_assumption = if (trend_type == "unit_trend") {
          "unit-specific linear trends"
        } else {
          "additive (level scale)"
        },
        trend_type = trend_type,
        trend_order = if (trend_type == "unit_trend") 1L else NA_integer_,
        transformation_applied = transformation_applied,
        outcome_type_input = ifelse(is.null(outcome_type), "rate (assumed)", outcome_type)
      )
    )
  }

  # EXTRACT FUNCTION: didimputation result → standard_estimate
  extract_fn <- function(result) {
    standard_estimate(
      att = result$estimate,
      se = result$std.error,
      model_name = "imputation",
      event_study = result$event_study,
      raw_result = result$raw,
      metadata = result$metadata
    )
  }

  estimator_adapter(
    name = "imputation",
    fit_fn = fit_fn,
    extract_fn = extract_fn,
    requires = "didimputation"
  )
}
