# R/estimate_models.R
#' Model-Agnostic DiD Estimation
#'
#' Estimates difference-in-differences models using registered adapters.
#' Uses the adapter pattern for clean separation between common parameters
#' and package-specific translation.
#'
#' @param data data.table or data.frame. Panel data
#' @param id_var Character. Unit identifier variable name
#' @param outcome_var Character. Outcome variable name
#' @param time_var Character. Time variable name
#' @param group_var Character. Treatment group/cohort variable name
#' @param controls Character vector. Control variable names (default NULL)
#' @param models_to_run Character vector. Model names to estimate (must be registered)
#' @param cluster_var Character. Clustering variable (default: same as id_var)
#' @param n_cores Integer. Number of cores for parallel estimation (default: detectCores()-1)
#' @param event_study Logical. Compute event study estimates? (default FALSE)
#' @param weightsname Character. Column name for observation weights (default NULL for unweighted)
#' @param outcome_type Character. Type of outcome variable: "rate" or "count".
#'   If NULL (default), assumes "rate" for backward compatibility.
#'   When mixing linear (cs, did2s, imputation) and Poisson (etwfe_poisson) estimators,
#'   this enables automatic transformation so each estimator uses appropriate data format.
#' @param pop_var Character. Population variable name. Required when:
#'   - outcome_type = "count" and running linear estimators (to compute rate)
#'   - outcome_type = "rate" and running Poisson estimators (to compute count)
#'   - Using etwfe with family = "poisson" (for offset)
#' @param family Character. Distribution family for etwfe. NULL (default) for linear/Gaussian,
#'   "poisson" for Poisson regression with multiplicative parallel trends assumption.
#'   Only affects etwfe/etwfe_poisson adapters; ignored by other estimators.
#' @param pretrend_test Logical. Whether to compute pre-trend tests (default FALSE).
#'   When TRUE, adapters will compute Wald tests on pre-treatment coefficients.
#' @param trend_type Character. Type of time trend assumption: "common" (default) for
#'   standard parallel trends, or "unit_trend" for unit-specific linear trends.
#'   **EXPERIMENTAL**: The "unit_trend" option has not been extensively tested and may
#'   cause numerical issues (near-singular matrices) with many units. Use with caution.
#'   Only affects imputation and etwfe_poisson adapters.
#' @param trend_order Integer. Polynomial order for trends (default 1 = linear).
#'   Currently only linear trends (order=1) are supported for unit_trend.
#' @param allow_unbalanced_panel Logical (default FALSE, matching \code{did}).
#'   Passed to \code{did::att_gt()} for the CS/etwfe adapters. When FALSE and the
#'   panel is unbalanced, \code{did} coerces to a balanced (complete-unit) panel
#'   by dropping units missing any period, which CHANGES THE ESTIMAND; the CS
#'   adapter warns in this case. Set TRUE to retain units with incomplete
#'   coverage. For imputation/did2s (no such argument), composition is instead
#'   controlled by the input sample -- see \code{\link{build_event_study_sample}}.
#' @param balance_e,min_e,max_e Event-study aggregation controls (default NULL =
#'   package default behavior). Forwarded to \code{did::aggte(type="dynamic")}
#'   for the CS adapter only. \code{balance_e} balances POST-treatment exposure
#'   (drops groups not observed through event time \code{balance_e}); \code{min_e}
#'   / \code{max_e} trim the displayed event-time window. NOTE: \code{balance_e}
#'   does not balance the pre-period; for a fully non-compositional event study
#'   (and for a fair CS-vs-imputation comparison) restrict the input sample with
#'   \code{\link{build_event_study_sample}} instead of relying on \code{balance_e}.
#' @param ... Additional arguments passed to adapters
#'
#' @return Named list of standard_estimate objects, one per model
#' @export
#'
#' @examples
#' \donttest{
#' # Estimate models using bundled dataset (requires 'did' or 'didimputation')
#' # if (requireNamespace("did", quietly = TRUE)) {
#' #   results <- estimate_models(
#' #     data = nfs_panel,
#' #     id_var = "state",
#' #     outcome_var = "assault_rate",
#' #     time_var = "year",
#' #     group_var = "treatment_year",
#' #     models_to_run = c("cs")
#' #   )
#' # }
#' }
estimate_models <- function(data,
                               id_var,
                               outcome_var,
                               time_var,
                               group_var,
                               controls = NULL,
                               models_to_run = c("cs", "imputation"),
                               cluster_var = NULL,
                               n_cores = NULL,
                               event_study = FALSE,
                               weightsname = NULL,
                               outcome_type = NULL,
                               pop_var = NULL,
                               family = NULL,
                               pretrend_test = FALSE,
                               trend_type = c("common", "cohort_trend"),
                               trend_order = 1L,
                               allow_unbalanced_panel = FALSE,
                               balance_e = NULL,
                               min_e = NULL,
                               max_e = NULL,
                               ...) {

  # Validate inputs
  stopifnot(
    is.data.frame(data),
    is.character(id_var),
    is.character(outcome_var),
    is.character(time_var),
    is.character(group_var)
  )

  # Validate trend parameters

  trend_type <- match.arg(trend_type)
  trend_order <- as.integer(trend_order)
  if (trend_type == "cohort_trend" && trend_order < 1L) {
    stop("trend_order must be >= 1 when trend_type = 'cohort_trend'")
  }

  # Default cluster to id_var
  if (is.null(cluster_var)) {
    cluster_var <- id_var
  }

  # Default cores
  if (is.null(n_cores)) {
    n_cores <- max(1, parallel::detectCores() - 1)
  }

  # Results list
  results <- list()

  # Loop over requested models
  for (model_name in models_to_run) {

    # Get adapter
    adapter <- get_adapter(model_name)

    # Check dependencies
    if (!check_adapter_deps(adapter)) {
      warning(sprintf("Skipping %s: required package '%s' not installed",
                      model_name, adapter$requires))
      next
    }

    # Wrap in tryCatch to handle per-model failures gracefully
    # This allows other estimators to run even if one fails
    model_result <- tryCatch({
      # Fit model (adapter translates common params → package call)
      raw_result <- adapter$fit(
        data = data,
        outcome_var = outcome_var,
        time_var = time_var,
        id_var = id_var,
        group_var = group_var,
        controls = controls,
        cluster_var = cluster_var,
        n_cores = n_cores,
        event_study = event_study,
        weightsname = weightsname,
        outcome_type = outcome_type,
        pop_var = pop_var,
        family = family,
        pretrend_test = pretrend_test,
        trend_type = trend_type,
        trend_order = trend_order,
        allow_unbalanced_panel = allow_unbalanced_panel,
        balance_e = balance_e,
        min_e = min_e,
        max_e = max_e,
        ...
      )

      # Extract to standard format
      adapter$extract(raw_result)

    }, error = function(e) {
      warning(sprintf("Model '%s' failed: %s", model_name, e$message))
      # Return a failed standard_estimate with NA values
      standard_estimate(
        att = NA_real_,
        se = NA_real_,
        model_name = model_name,
        event_study = NULL,
        raw_result = NULL,
        metadata = list(
          error = TRUE,
          error_message = e$message
        )
      )
    })

    # Store result (even if failed, to track what happened)
    results[[model_name]] <- model_result
  }

  results
}
