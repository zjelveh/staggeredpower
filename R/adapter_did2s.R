# R/adapter_did2s.R
#' Gardner Two-Stage Difference-in-Differences (2SDID) Adapter
#'
#' Translates between staggeredpower's common interface and the `did2s` package.
#' This implements the Gardner (2022) two-stage DiD estimator, which is robust
#' to heterogeneous treatment effects and staggered adoption.
#'
#' @return An estimator_adapter for 2SDID estimation
#' @export
#'
#' @examples
#' \dontrun{
#' adapter <- adapter_did2s()
#' result <- adapter$fit(data = my_data, outcome_var = "y", ...)
#' std_result <- adapter$extract(result)
#' }
adapter_did2s <- function() {

  # FIT FUNCTION: Common params → did2s::did2s() call
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
                     treat_var = NULL,
                     ...) {

    # Default cluster to id_var
    if (is.null(cluster_var)) {
      cluster_var <- id_var
    }

    # Create treatment indicator if not provided
    # did2s expects a 0/1 treatment variable
    df <- as.data.frame(data)
    if (is.null(treat_var)) {
      # Create treatment indicator: 1 if year >= group_var (treatment year)
      df$.treat <- as.integer(df[[time_var]] >= df[[group_var]] & df[[group_var]] > 0)
      treat_var <- ".treat"
    }

    # Build first_stage formula: ~ controls | unit + time FEs
    if (!is.null(controls) && length(controls) > 0) {
      first_stage <- as.formula(
        paste0("~ ", paste(controls, collapse = " + "), " | ", id_var, " + ", time_var)
      )
    } else {
      first_stage <- as.formula(
        paste0("~ 0 | ", id_var, " + ", time_var)
      )
    }

    # Second stage: treatment effect
    second_stage <- as.formula(paste0("~ i(", treat_var, ")"))

    # Handle weights - did2s expects a vector, not a column name
    weights_vec <- NULL
    if (!is.null(weightsname) && weightsname %in% names(df)) {
      weights_vec <- df[[weightsname]]
    }

    # Call did2s::did2s
    result <- did2s::did2s(
      data = df,
      yname = outcome_var,
      first_stage = first_stage,
      second_stage = second_stage,
      treatment = treat_var,
      cluster_var = cluster_var,
      weights = weights_vec,
      verbose = FALSE
    )

    # Extract ATT and SE from the fixest result
    # The coefficient on the treatment indicator is the ATT
    coef_table <- as.data.frame(summary(result)$coeftable)
    treat_row <- grep(treat_var, rownames(coef_table), value = TRUE)[1]

    att <- coef_table[treat_row, "Estimate"]
    se <- coef_table[treat_row, "Std. Error"]

    # Event study if requested
    event_study_result <- NULL
    if (event_study) {
      # For event study, use relative time indicators
      # Create relative time variable
      df$.rel_time <- df[[time_var]] - df[[group_var]]
      # Cap at reasonable bounds
      df$.rel_time <- pmax(pmin(df$.rel_time, 10), -6)

      # Second stage with event time dummies (reference = -1)
      second_stage_es <- as.formula("~ i(.rel_time, ref = -1)")

      result_es <- tryCatch({
        did2s::did2s(
          data = df,
          yname = outcome_var,
          first_stage = first_stage,
          second_stage = second_stage_es,
          treatment = treat_var,
          cluster_var = cluster_var,
          weights = weights_vec,
          verbose = FALSE
        )
      }, error = function(e) NULL)

      if (!is.null(result_es)) {
        es_coef <- as.data.frame(summary(result_es)$coeftable)
        # Parse relative time from coefficient names
        rel_times <- as.numeric(gsub(".*::(-?[0-9]+).*", "\\1", rownames(es_coef)))
        event_study_result <- data.table::data.table(
          rel_time = rel_times,
          att = es_coef[, "Estimate"],
          se = es_coef[, "Std. Error"]
        )
      }
    }

    list(
      att = att,
      se = se,
      event_study = event_study_result,
      raw = result
    )
  }

  # EXTRACT FUNCTION: did2s result → standard_estimate
  extract_fn <- function(result) {
    standard_estimate(
      att = result$att,
      se = result$se,
      model_name = "did2s",
      event_study = result$event_study,
      raw_result = result$raw,
      metadata = list(
        method = "two_stage_did",
        package = "did2s",
        reference = "Gardner (2022)"
      )
    )
  }

  estimator_adapter(
    name = "did2s",
    fit_fn = fit_fn,
    extract_fn = extract_fn,
    requires = "did2s"
  )
}
