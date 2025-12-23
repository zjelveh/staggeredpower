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

    # Call didimputation::did_imputation
    # horizon = NULL gives static ATT, horizon = TRUE gives all event times
    result <- didimputation::did_imputation(
      data = as.data.frame(data),
      yname = working_outcome,
      gname = group_var,
      tname = time_var,
      idname = id_var,
      first_stage = first_stage_formula,
      wname = weightsname,
      cluster_var = cluster_var,
      horizon = if (event_study) TRUE else NULL,
      pretrends = if (event_study) TRUE else NULL
    )

    # The result is already aggregated by did_imputation
    # Extract the overall ATT
    list(
      estimate = result$estimate,
      std.error = result$std.error,
      raw = result,
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
      event_study = NULL,  # didimputation doesn't return event study in same format
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
