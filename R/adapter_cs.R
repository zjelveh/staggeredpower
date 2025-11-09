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
                     ...) {

    # Default cluster to id_var
    if (is.null(cluster_var)) {
      cluster_var <- id_var
    }

    # Default cores to all available - 1
    if (is.null(n_cores)) {
      n_cores <- max(1, parallel::detectCores() - 1)
    }

    # Translate controls to xformla
    if (!is.null(controls) && length(controls) > 0) {
      control_formula <- as.formula(paste0(" ~ ", paste(controls, collapse = " + ")))
    } else {
      control_formula <- as.formula(" ~ 1")
    }

    # Call did::att_gt
    m_csa <- did::att_gt(
      yname = outcome_var,
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
      base_period = "varying",
      print_details = FALSE
    )

    # Aggregate to overall ATT
    m_csa_agg <- did::aggte(m_csa, type = "simple", na.rm = TRUE)

    # Optionally compute event study
    event_study_result <- NULL
    if (event_study) {
      event_study_agg <- did::aggte(m_csa, type = "dynamic", na.rm = TRUE)
      event_study_result <- data.table::data.table(
        rel_time = event_study_agg$egt,
        att = event_study_agg$att.egt,
        se = event_study_agg$se.egt
      )
    }

    # Return both for extraction
    list(
      agg = m_csa_agg,
      event_study = event_study_result,
      raw = m_csa
    )
  }

  # EXTRACT FUNCTION: did result → standard_estimate
  extract_fn <- function(result) {
    standard_estimate(
      att = result$agg$overall.att,
      se = result$agg$overall.se,
      model_name = "cs",
      event_study = result$event_study,
      raw_result = result$raw,
      metadata = list(
        control_group = "notyettreated",
        estimator = "doubly_robust"
      )
    )
  }

  estimator_adapter(
    name = "cs",
    fit_fn = fit_fn,
    extract_fn = extract_fn,
    requires = "did"
  )
}
