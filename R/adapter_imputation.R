# R/adapter_imputation.R
#' Borusyak-Jaravel-Spiess Imputation Adapter
#'
#' Translates between staggeredpower's common interface and the `didimputation` package
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
                     ...) {

    # Default cluster to id_var
    if (is.null(cluster_var)) {
      cluster_var <- id_var
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
    result <- didimputation::did_imputation(
      data = as.data.frame(data),
      yname = outcome_var,
      gname = group_var,
      tname = time_var,
      idname = id_var,
      first_stage = first_stage_formula,
      wname = weightsname,
      cluster_var = cluster_var,
      horizon = event_study,
      pretrends = if (event_study) -6:0 else FALSE
    )

    # The result is already aggregated by did_imputation
    # Extract the overall ATT
    list(
      estimate = result$estimate,
      std.error = result$std.error,
      raw = result
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
      metadata = list(
        method = "imputation",
        package = "didimputation"
      )
    )
  }

  estimator_adapter(
    name = "imputation",
    fit_fn = fit_fn,
    extract_fn = extract_fn,
    requires = "didimputation"
  )
}
