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
                     outcome_type = NULL,
                     pop_var = NULL,
                     family = NULL,
                     pretrend_test = FALSE,
                     ...) {

    # Default cluster to id_var
    if (is.null(cluster_var)) {
      cluster_var <- id_var
    }

    # Convert to data.table for manipulation
    data <- data.table::as.data.table(data)

    # Handle count → rate transformation if needed
    working_outcome <- outcome_var
    transformation_applied <- "none"

    if (!is.null(outcome_type) && outcome_type == "count") {
      if (is.null(pop_var)) {
        stop("did2s adapter with outcome_type='count' requires pop_var to compute rate")
      }
      # Create rate variable: rate = count / pop * 100000
      data[, .did2s_rate := get(outcome_var) / get(pop_var) * 100000]
      working_outcome <- ".did2s_rate"
      transformation_applied <- "count_to_rate"
    }

    # Create treatment indicator if not provided
    # did2s expects a 0/1 treatment variable
    df <- as.data.frame(data)
    if (is.null(treat_var)) {
      # Create treatment indicator: 1 if year >= group_var (treatment year)
      # Handle NA values in group_var (never-treated units) - they get treat = 0
      df$.treat <- as.integer(
        !is.na(df[[group_var]]) &
        df[[group_var]] > 0 &
        df[[time_var]] >= df[[group_var]]
      )
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
      yname = working_outcome,
      first_stage = first_stage,
      second_stage = second_stage,
      treatment = treat_var,
      cluster_var = cluster_var,
      weights = weights_vec,
      verbose = FALSE
    )

    # Extract ATT and SE from the fixest result
    # The coefficient on treatment = 1 is the ATT (not treatment = 0)
    coef_table <- as.data.frame(summary(result)$coeftable)
    # Match .treat::1 specifically (the treated coefficient, not .treat::0)
    treat_row <- grep(paste0(treat_var, "::1"), rownames(coef_table), value = TRUE)[1]

    att <- coef_table[treat_row, "Estimate"]
    se <- coef_table[treat_row, "Std. Error"]

    # Event study if requested
    event_study_result <- NULL
    result_es <- NULL
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

    # Pre-trend test if requested
    pt_result <- NULL
    if (pretrend_test) {
      # Check if event study was run
      if (is.null(result_es)) {
        pt_result <- list(
          p_value = NA_real_,
          wald_stat = NA_real_,
          df = NA_integer_,
          reject_at_05 = NA,
          warning = "Pre-trend test requires event_study = TRUE",
          method = "event_study"
        )
      } else {
      # Extract pre-treatment terms from vcov
      full_vcov <- stats::vcov(result_es)
      all_terms <- rownames(full_vcov)
      all_coefs <- names(stats::coef(result_es))

      # Find pre-treatment terms (negative relative time, excluding -1 reference)
      # Pattern matches both simple names and __CLEAN__ variants
      pre_pattern <- "rel_time::-[0-9]+"
      pre_coef_names <- grep(pre_pattern, all_coefs, value = TRUE)
      pre_coef_names <- pre_coef_names[!grepl("::-1$", pre_coef_names)]

      # Match vcov names (may have __CLEAN__ prefix)
      pre_vcov_names <- grep(pre_pattern, all_terms, value = TRUE)
      pre_vcov_names <- pre_vcov_names[!grepl("::-1$", pre_vcov_names)]

      if (length(pre_coef_names) > 0 && length(pre_vcov_names) > 0) {
        pre_coefs <- stats::coef(result_es)[pre_coef_names]
        pre_vcov_sub <- full_vcov[pre_vcov_names, pre_vcov_names, drop = FALSE]

        pt_result <- compute_pretrend_wald_test(pre_coefs, pre_vcov_sub)
        pt_result$method <- "event_study"
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
      method = "two_stage_did",
      package = "did2s",
      reference = "Gardner (2022)",
      pt_assumption = "additive (level scale)",
      transformation_applied = transformation_applied,
      outcome_type_input = ifelse(is.null(outcome_type), "rate (assumed)", outcome_type)
    )

    # Add pretrend_test to metadata if computed
    if (!is.null(pt_result)) {
      metadata$pretrend_test <- pt_result
    }

    list(
      att = att,
      se = se,
      event_study = event_study_result,
      raw = result,
      metadata = metadata
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
      metadata = result$metadata
    )
  }

  estimator_adapter(
    name = "did2s",
    fit_fn = fit_fn,
    extract_fn = extract_fn,
    requires = "did2s"
  )
}
