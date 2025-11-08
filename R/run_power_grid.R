# R/run_power_grid.R
#' Run Power Analysis Grid Search
#'
#' @description
#' Convenience wrapper around run_power_analysis() that runs power analysis
#' across multiple parameter combinations. Accepts vectors for key parameters
#' and iterates over all combinations.
#'
#' @param data_clean Clean panel dataset (same as run_power_analysis)
#' @param unit_var Unit identifier column name (same as run_power_analysis)
#' @param group_var Treatment cohort column name (same as run_power_analysis)
#' @param time_var Time variable column name (same as run_power_analysis)
#' @param outcome Outcome variable column name (same as run_power_analysis)
#' @param pta_type Vector of PTA types to test. Default: c("cs", "imputation")
#' @param enforce_type List of enforce_type specifications. Each element can be NULL or character vector of controls.
#'   Default: list(NULL)
#' @param percent_effect Vector of effect sizes to test. Default: seq(0.05, 0.20, 0.05)
#' @param transform_outcome Vector of outcome transformations. Default: c(NULL, "log")
#' @param controls List of control variable sets. Each element is a character vector or NULL.
#'   Default: list(NULL)
#' @param models_to_run Models to estimate (same as run_power_analysis). Default: c("cs", "imputation")
#' @param n_sims Number of simulations per specification (same as run_power_analysis). Default: 100
#' @param parallel Whether to use parallel processing. Default: FALSE
#' @param n_cores Number of cores if parallel=TRUE. Default: NULL (uses all available - 1)
#'
#' @return A list with two components:
#' \describe{
#'   \item{final_power}{data.table with power analysis results for all specifications}
#'   \item{specifications}{data.table summarizing the grid of specifications run}
#' }
#'
#' @examples
#' \dontrun{
#' # Test multiple effect sizes and PTA methods
#' results <- run_power_grid(
#'   data_clean = my_data,
#'   unit_var = "state_fips",
#'   group_var = "year_passed",
#'   time_var = "year",
#'   outcome = "dv_rate",
#'   pta_type = c("cs", "imputation"),
#'   percent_effect = seq(0.05, 0.15, 0.05),
#'   n_sims = 100
#' )
#'
#' # With multiple control sets
#' results <- run_power_grid(
#'   data_clean = my_data,
#'   unit_var = "state_fips",
#'   group_var = "year_passed",
#'   time_var = "year",
#'   outcome = "dv_rate",
#'   controls = list(
#'     NULL,
#'     c("unemp_rate"),
#'     c("unemp_rate", "crime_rate")
#'   ),
#'   percent_effect = c(0.10, 0.15),
#'   n_sims = 100
#' )
#' }
#'
#' @export
run_power_grid <- function(data_clean,
                          unit_var,
                          group_var,
                          time_var,
                          outcome,
                          pta_type = c("cs", "imputation"),
                          enforce_type = list(NULL),
                          percent_effect = seq(0.05, 0.20, 0.05),
                          transform_outcome = list(NULL),
                          controls = list(NULL),
                          models_to_run = c("cs", "imputation"),
                          n_sims = 100,
                          parallel = FALSE,
                          n_cores = NULL) {

  # Load required packages
  require(data.table)

  # Create grid of specifications
  # Handle NULL values in lists by converting to character "NULL"
  enforce_type_char <- lapply(enforce_type, function(x) {
    if(is.null(x)) return("NULL_CONTROLS")
    paste(x, collapse = "*")
  })

  transform_outcome_char <- lapply(transform_outcome, function(x) {
    if(is.null(x)) return("NULL_TRANSFORM")
    x
  })

  controls_char <- lapply(controls, function(x) {
    if(is.null(x)) return("NULL_CONTROLS")
    paste(x, collapse = "*")
  })

  # Create grid
  grid <- expand.grid(
    pta_type = pta_type,
    enforce_type = unlist(enforce_type_char),
    percent_effect = percent_effect,
    transform_outcome = unlist(transform_outcome_char),
    controls = unlist(controls_char),
    stringsAsFactors = FALSE
  )

  grid <- data.table(grid)
  grid[, spec_id := .I]

  cat(sprintf("Running %d specifications...\n", nrow(grid)))

  # Function to run single specification
  run_single_spec <- function(spec_row) {
    # Convert back from character to actual values
    curr_enforce <- if(spec_row$enforce_type == "NULL_CONTROLS") {
      NULL
    } else {
      strsplit(spec_row$enforce_type, "\\*")[[1]]
    }

    curr_transform <- if(spec_row$transform_outcome == "NULL_TRANSFORM") {
      NULL
    } else {
      spec_row$transform_outcome
    }

    curr_controls <- if(spec_row$controls == "NULL_CONTROLS") {
      NULL
    } else {
      strsplit(spec_row$controls, "\\*")[[1]]
    }

    # Print progress
    cat(sprintf("Running spec %d/%d: pta=%s, effect=%.2f, controls=%s\n",
                spec_row$spec_id, nrow(grid),
                spec_row$pta_type, spec_row$percent_effect,
                spec_row$controls))

    # Run power analysis
    tryCatch({
      result <- run_power_analysis(
        data_clean = data_clean,
        unit_var = unit_var,
        group_var = group_var,
        time_var = time_var,
        outcome = outcome,
        pta_type = spec_row$pta_type,
        enforce_type = curr_enforce,
        percent_effect = spec_row$percent_effect,
        transform_outcome = curr_transform,
        controls = curr_controls,
        models_to_run = models_to_run,
        n_sims = n_sims
      )

      # Add specification identifiers to results
      result$final_power[, spec_id := spec_row$spec_id]

      return(result$final_power)
    }, error = function(e) {
      warning(sprintf("Specification %d failed: %s", spec_row$spec_id, e$message))
      return(NULL)
    })
  }

  # Run across grid
  if(parallel) {
    require(foreach)
    require(doParallel)

    if(is.null(n_cores)) {
      n_cores <- parallel::detectCores() - 1
    }

    cl <- makeCluster(n_cores)
    registerDoParallel(cl)

    cat(sprintf("Running in parallel with %d cores...\n", n_cores))

    results_list <- foreach(i = 1:nrow(grid),
                           .packages = c("data.table", "staggeredpower")) %dopar% {
      run_single_spec(grid[i])
    }

    stopCluster(cl)
  } else {
    # Sequential execution
    results_list <- lapply(1:nrow(grid), function(i) {
      run_single_spec(grid[i])
    })
  }

  # Combine results
  results_combined <- rbindlist(results_list[!sapply(results_list, is.null)])

  # Merge with specification details
  results_combined <- merge(results_combined, grid, by = "spec_id")

  # Calculate summary statistics
  power_summary <- results_combined[, .(
    power = mean(abs(att/se) > 1.96),
    mean_att = mean(att),
    mean_se = mean(se),
    n_sims = .N / length(unique(model)),
    mean_units_dropped = mean(n_dropped_units, na.rm = TRUE)
  ), by = .(spec_id, pta_type, enforce_type, percent_effect,
            transform_outcome, controls, model)]

  cat("\nGrid search complete!\n")
  cat(sprintf("Successfully ran %d specifications\n",
              length(unique(results_combined$spec_id))))

  return(list(
    final_power = results_combined,
    power_summary = power_summary,
    specifications = grid
  ))
}
