# R/adapters/adapter_base.R
#' Base Adapter Class for DiD Estimators
#'
#' Provides a common interface for translating between staggeredpower's
#' model-agnostic parameters and package-specific estimation functions.
#'
#' @param name Character. Unique identifier for this estimator
#' @param fit_fn Function. Translates common params → package call, returns raw result
#' @param extract_fn Function. Translates raw result → standard_estimate object
#' @param requires Character vector. Package dependencies
#'
#' @return An estimator_adapter object
#' @export
estimator_adapter <- function(name, fit_fn, extract_fn, requires = NULL) {
  # Validate inputs
  stopifnot(
    is.character(name),
    length(name) == 1,
    is.function(fit_fn),
    is.function(extract_fn)
  )

  structure(
    list(
      name = name,
      fit = fit_fn,
      extract = extract_fn,
      requires = requires
    ),
    class = "estimator_adapter"
  )
}

#' Standard Estimate Format
#'
#' All adapters must return results in this format
#'
#' @param att Numeric. Average treatment effect on treated
#' @param se Numeric. Standard error
#' @param model_name Character. Model identifier
#' @param event_study Optional. Event study results (data.table)
#' @param raw_result Optional. Original package output for debugging
#' @param metadata Optional. Additional model-specific info
#'
#' @return A standard_estimate object
#' @export
standard_estimate <- function(att, se, model_name,
                              event_study = NULL,
                              raw_result = NULL,
                              metadata = list()) {
  structure(
    list(
      agg = list(
        att = att,
        se = se,
        model = model_name
      ),
      event_study = event_study,
      raw = raw_result,
      metadata = metadata
    ),
    class = "standard_estimate"
  )
}

#' Adapter Registry
#'
#' Global registry for storing estimator adapters
.adapter_registry <- new.env(parent = emptyenv())

#' Register an Adapter
#'
#' @param adapter An estimator_adapter object
#' @export
register_adapter <- function(adapter) {
  stopifnot(inherits(adapter, "estimator_adapter"))
  .adapter_registry[[adapter$name]] <- adapter
  invisible(adapter)
}

#' Get Registered Adapter
#'
#' @param name Character. Adapter name
#' @return An estimator_adapter object
#' @export
get_adapter <- function(name) {
  adapter <- .adapter_registry[[name]]
  if (is.null(adapter)) {
    stop(sprintf("No adapter registered for model: %s\nAvailable: %s",
                 name, paste(list_adapters(), collapse = ", ")))
  }
  adapter
}

#' List All Registered Adapters
#'
#' @return Character vector of adapter names
#' @export
list_adapters <- function() {
  ls(.adapter_registry)
}

#' Check Package Dependencies
#'
#' @param adapter An estimator_adapter object
#' @return TRUE if dependencies available, throws error otherwise
check_adapter_deps <- function(adapter) {
  if (!is.null(adapter$requires)) {
    for (pkg in adapter$requires) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        stop(sprintf("Adapter '%s' requires package '%s' but it is not installed.\nInstall with: install.packages('%s')",
                     adapter$name, pkg, pkg))
      }
    }
  }
  TRUE
}

#' Clear Adapter Registry (For Testing)
#'
#' @keywords internal
clear_adapter_registry <- function() {
  rm(list = ls(.adapter_registry), envir = .adapter_registry)
  invisible(NULL)
}
