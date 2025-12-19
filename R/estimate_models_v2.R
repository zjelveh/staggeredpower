# R/estimate_models_v2.R
#' Model-Agnostic DiD Estimation (v2 with Adapter Pattern)
#'
#' Estimates difference-in-differences models using registered adapters.
#' This is the v2 implementation that uses the adapter pattern for
#' clean separation between common parameters and package-specific translation.
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
#' @param ... Additional arguments passed to adapters
#'
#' @return Named list of standard_estimate objects, one per model
#' @export
#'
#' @examples
#' \dontrun{
#' results <- estimate_models_v2(
#'   data = my_data,
#'   id_var = "state",
#'   outcome_var = "y",
#'   time_var = "year",
#'   group_var = "cohort",
#'   models_to_run = c("cs", "imputation")
#' )
#' results$cs$agg$att  # Callaway-Sant'Anna ATT
#' results$imputation$agg$att  # Imputation ATT
#' }
estimate_models_v2 <- function(data,
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
                               ...) {

  # Validate inputs
  stopifnot(
    is.data.frame(data),
    is.character(id_var),
    is.character(outcome_var),
    is.character(time_var),
    is.character(group_var)
  )

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
    cat(sprintf("Estimating %s model...\n", model_name))

    # Get adapter
    adapter <- get_adapter(model_name)

    # Check dependencies
    if (!check_adapter_deps(adapter)) {
      warning(sprintf("Skipping %s: required package '%s' not installed",
                      model_name, adapter$requires))
      next
    }

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
      ...
    )

    # Extract to standard format
    std_result <- adapter$extract(raw_result)

    # Store result
    results[[model_name]] <- std_result
  }

  results
}
