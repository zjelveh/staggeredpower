# tests/testthat/test-power-analysis-v2.R
# Test power analysis with v2 adapter pattern

test_that("run_power_analysis works with use_v2=TRUE", {
  skip("Requires parallel backend setup - tested in examples/")
  skip_if_not_installed("did")
  skip_if_not_installed("didimputation")
  skip_if_not_installed("foreach")

  # Load real strangulation data
  tryCatch({
    setwd('/home/zjelveh/Dropbox/research/base_rate_fallacy/repo/base_rate_fallacy')
    source('code/paper_code/estimation/create_datasets.R')
    datasets <- create_datasets()
    data <- datasets$state
  }, error = function(e) {
    skip(paste("Could not load strangulation data:", e$message))
  })

  # Convert to data.table
  data <- data.table::as.data.table(data)

  outcome_var <- "y_nibrs_female_aggshare"

  if (!outcome_var %in% names(data)) {
    skip(paste("Outcome variable", outcome_var, "not found in data"))
  }

  # Create treat_ind variable: 1 if treated (year >= year_passed), 0 otherwise
  data[, treat_ind := ifelse(!is.na(year_passed) & year >= year_passed, 1, 0)]
  data[, rel_pass := ifelse(!is.na(year_passed), year - year_passed, NA)]

  # Small power analysis: 3 simulations, CS only
  results <- run_power_analysis(
    data_clean = data,
    unit_var = "state_fips",
    group_var = "year_passed",
    time_var = "year",
    rel_pass_var = "rel_pass",
    treat_ind_var = "treat_ind",
    controls = NULL,
    outcome = outcome_var,
    transform_outcome = NULL,
    pta_type = "cs",
    enforce_type = NULL,
    percent_effect = 1.1,  # 10% increase
    models_to_run = "cs",
    n_sims = 3,
    use_v2 = TRUE  # Use adapter pattern
  )

  # Check structure
  expect_type(results, "list")
  expect_true("final_power" %in% names(results))
  expect_s3_class(results$final_power, "data.table")

  # Check that we have results
  expect_true(nrow(results$final_power) > 0)

  # Check columns
  expect_true("att" %in% names(results$final_power))
  expect_true("se" %in% names(results$final_power))
  expect_true("model" %in% names(results$final_power))

  # Check that att and se are numeric and not NA
  expect_type(results$final_power$att, "double")
  expect_type(results$final_power$se, "double")
  expect_false(any(is.na(results$final_power$att)))
  expect_false(any(is.na(results$final_power$se)))
})

test_that("run_power_analysis maintains backward compatibility with use_v2=FALSE", {
  skip("Requires parallel backend setup - tested in examples/")
  skip_if_not_installed("did")
  skip_if_not_installed("didimputation")
  skip_if_not_installed("foreach")

  # Load real strangulation data
  tryCatch({
    setwd('/home/zjelveh/Dropbox/research/base_rate_fallacy/repo/base_rate_fallacy')
    source('code/paper_code/estimation/create_datasets.R')
    datasets <- create_datasets()
    data <- datasets$state
  }, error = function(e) {
    skip(paste("Could not load strangulation data:", e$message))
  })

  # Convert to data.table
  data <- data.table::as.data.table(data)

  outcome_var <- "y_nibrs_female_aggshare"

  if (!outcome_var %in% names(data)) {
    skip(paste("Outcome variable", outcome_var, "not found in data"))
  }

  # Create treat_ind variable: 1 if treated (year >= year_passed), 0 otherwise
  data[, treat_ind := ifelse(!is.na(year_passed) & year >= year_passed, 1, 0)]
  data[, rel_pass := ifelse(!is.na(year_passed), year - year_passed, NA)]

  # Small power analysis with original code path
  results <- run_power_analysis(
    data_clean = data,
    unit_var = "state_fips",
    group_var = "year_passed",
    time_var = "year",
    rel_pass_var = "rel_pass",
    treat_ind_var = "treat_ind",
    controls = NULL,
    outcome = outcome_var,
    transform_outcome = NULL,
    pta_type = "cs",
    enforce_type = NULL,
    percent_effect = 1.1,
    models_to_run = "cs",
    n_sims = 3,
    use_v2 = FALSE  # Use original estimate_models
  )

  # Check structure
  expect_type(results, "list")
  expect_true("final_power" %in% names(results))
  expect_s3_class(results$final_power, "data.table")

  # Check that we have results
  expect_true(nrow(results$final_power) > 0)

  # Check columns
  expect_true("att" %in% names(results$final_power))
  expect_true("se" %in% names(results$final_power))

  # Check that att and se are numeric and not NA
  expect_type(results$final_power$att, "double")
  expect_type(results$final_power$se, "double")
  expect_false(any(is.na(results$final_power$att)))
  expect_false(any(is.na(results$final_power$se)))
})

test_that("use_v2 parameter is accepted and backward compatible", {
  # Simple smoke test that use_v2 parameter doesn't break function signature
  # Full power analysis integration tests require parallel backend setup

  expect_true("use_v2" %in% names(formals(run_power_analysis)))

  # Verify default is FALSE for backward compatibility
  expect_equal(formals(run_power_analysis)$use_v2, FALSE)
})
