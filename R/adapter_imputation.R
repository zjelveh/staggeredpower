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
#' \dontrun{
#' adapter <- adapter_imputation()
#' result <- adapter$fit(data = my_data, outcome_var = "y", ...)
#' std_result <- adapter$extract(result)
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
                     ...) {

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

    # Build first_stage formula for controls
    if (!is.null(controls) && length(controls) > 0) {
      first_stage_formula <- as.formula(
        paste0("~ 0 + ", paste(controls, collapse = " + "))
      )
    } else {
      first_stage_formula <- NULL
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
        # Parse term column: "horizon::3" → rel_time = 3
        rel_times <- as.integer(gsub(".*::(-?[0-9]+)$", "\\1", es_raw$term))
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
      std.error = result_att$std.error[1],
      event_study = event_study_result,
      raw = result_att,
      metadata = list(
        method = "imputation",
        package = "didimputation",
        pt_assumption = "additive (level scale)",
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
