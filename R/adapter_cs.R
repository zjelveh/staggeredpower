# R/adapter_cs.R
#' Callaway-Sant'Anna Adapter
#'
#' Translates between staggeredpower's common interface and the `did` package
#'
#' @return An estimator_adapter for Callaway-Sant'Anna estimation
#' @export
#'
#' @examples
#' \dontrun{
#' adapter <- adapter_cs()
#' result <- adapter$fit(data = my_data, outcome_var = "y", ...)
#' std_result <- adapter$extract(result)
#' }
adapter_cs <- function() {

  # FIT FUNCTION: Common params → did::att_gt() call
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
                     pretrend_test = FALSE,
                     outcome_type = NULL,
                     pop_var = NULL,
                     base_period = "varying",
                     allow_unbalanced_panel = FALSE,
                     family = NULL,  # ignored by CS, but accepted for compatibility
                     ...) {

    # Convert to data.table for manipulation
    data <- data.table::as.data.table(data)

    # did package requires never-treated units to be coded as 0, not NA
    # NA values are dropped as "missing data", losing control observations
    if (any(is.na(data[[group_var]]))) {
      data[is.na(get(group_var)), (group_var) := 0L]
    }

    # Default cluster to id_var
    if (is.null(cluster_var)) {
      cluster_var <- id_var
    }

    # Default cores to all available - 1
    if (is.null(n_cores)) {
      n_cores <- max(1, parallel::detectCores() - 1)
    }

    # Handle count → rate transformation if needed
    working_outcome <- outcome_var
    transformation_applied <- "none"

    if (!is.null(outcome_type) && outcome_type == "count") {
      if (is.null(pop_var)) {
        stop("CS adapter with outcome_type='count' requires pop_var to compute rate")
      }
      # Create rate variable: rate = count / pop * 100000
      data[, .cs_rate := get(outcome_var) / get(pop_var) * 100000]
      working_outcome <- ".cs_rate"
      transformation_applied <- "count_to_rate"
    }

    # Translate controls to xformla
    if (!is.null(controls) && length(controls) > 0) {
      control_formula <- as.formula(paste0(" ~ ", paste(controls, collapse = " + ")))
    } else {
      control_formula <- as.formula(" ~ 1")
    }

    # Call did::att_gt
    m_csa <- did::att_gt(
      yname = working_outcome,
      xformla = control_formula,
      tname = time_var,
      idname = id_var,
      gname = group_var,
      data = as.data.frame(data),  # did expects data.frame
      cores = n_cores,
      control_group = "notyettreated",
      anticipation = 0,
      est_method = "dr",
      clustervars = cluster_var,
      base_period = base_period,
      allow_unbalanced_panel = allow_unbalanced_panel,
      print_details = FALSE,
      weightsname = weightsname
    )

    # Aggregate to overall ATT
    m_csa_agg <- did::aggte(m_csa, type = "simple", na.rm = TRUE)

    # Optionally compute event study
    event_study_result <- NULL
    event_study_agg <- NULL
    if (event_study) {
      event_study_agg <- did::aggte(m_csa, type = "dynamic", na.rm = TRUE)
      event_study_result <- data.table::data.table(
        rel_time = event_study_agg$egt,
        att = event_study_agg$att.egt,
        se = event_study_agg$se.egt
      )
    }

    # Pre-trend test if requested
    pt_result <- NULL
    if (pretrend_test) {
      if (!event_study) {
        # Can't do pre-trend test without event study
        pt_result <- list(
          p_value = NA_real_,
          wald_stat = NA_real_,
          df = NA_integer_,
          reject_at_05 = NA,
          warning = "Pre-trend test requires event_study = TRUE",
          method = "event_study"
        )
      } else {
        # Extract pre-treatment periods (negative event times, excluding reference period)
        pre_idx <- event_study_agg$egt < 0

        if (sum(pre_idx) > 0) {
          # Extract pre-treatment coefficients
          pre_coefs <- event_study_agg$att.egt[pre_idx]
          names(pre_coefs) <- paste0("t", event_study_agg$egt[pre_idx])

          # Reconstruct VCV from influence functions
          # The influence functions are in event_study_agg$inf.function$dynamic.inf.func.e
          # This is a matrix with rows = observations, columns = event times
          # Note: This may produce slightly smaller variances than did's reported SEs
          # due to different small-sample corrections. The resulting test is conservative.
          pt_result <- tryCatch({
            inf_funcs <- event_study_agg$inf.function$dynamic.inf.func.e

            # Extract influence functions for pre-treatment periods
            pre_inf_funcs <- inf_funcs[, pre_idx, drop = FALSE]

            # Compute VCV as cov(inf_functions) / n where n is number of clusters
            # For CS, the number of observations equals the number of clusters
            # since influence functions are already aggregated at cluster level
            n_clusters <- nrow(pre_inf_funcs)
            pre_vcov <- stats::cov(pre_inf_funcs) / n_clusters
            rownames(pre_vcov) <- colnames(pre_vcov) <- names(pre_coefs)

            # Compute Wald test
            result <- compute_pretrend_wald_test(pre_coefs, pre_vcov)
            result$method <- "event_study"
            result$vcov_note <- "Reconstructed from influence functions"
            result  # Return the result
          }, error = function(e) {
            # Fallback: use diagonal approximation if influence functions fail
            pre_vcov <- diag(event_study_agg$se.egt[pre_idx]^2)
            rownames(pre_vcov) <- colnames(pre_vcov) <- names(pre_coefs)

            result <- compute_pretrend_wald_test(pre_coefs, pre_vcov)
            result$method <- "event_study"
            result$vcov_note <- paste0("Diagonal approximation (influence functions unavailable: ", e$message, ")")
            result  # Return the result
          })
        } else {
          pt_result <- list(
            p_value = NA_real_,
            wald_stat = NA_real_,
            df = NA_integer_,
            reject_at_05 = NA,
            warning = "No pre-treatment periods found in event study",
            method = "event_study"
          )
        }
      }
    }

    # Build metadata
    metadata <- list(
      control_group = "notyettreated",
      estimator = "doubly_robust",
      pt_assumption = "additive (level scale)",
      transformation_applied = transformation_applied,
      outcome_type_input = ifelse(is.null(outcome_type), "rate (assumed)", outcome_type)
    )

    # Add pretrend_test to metadata if computed
    if (!is.null(pt_result)) {
      metadata$pretrend_test <- pt_result
    }

    # Return both for extraction
    list(
      agg = m_csa_agg,
      event_study = event_study_result,
      raw = m_csa,
      metadata = metadata
    )
  }

  # EXTRACT FUNCTION: did result → standard_estimate
  extract_fn <- function(result) {
    # Use metadata from fit result (may include pretrend_test)
    metadata <- if (!is.null(result$metadata)) {
      result$metadata
    } else {
      list(
        control_group = "notyettreated",
        estimator = "doubly_robust"
      )
    }

    standard_estimate(
      att = result$agg$overall.att,
      se = result$agg$overall.se,
      model_name = "cs",
      event_study = result$event_study,
      raw_result = result$raw,
      metadata = metadata
    )
  }

  estimator_adapter(
    name = "cs",
    fit_fn = fit_fn,
    extract_fn = extract_fn,
    requires = "did"
  )
}
