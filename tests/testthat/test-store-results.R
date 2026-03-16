test_that("store_results works with generic column names", {
  mock_results <- list(
    cs = list(
      agg = list(overall.att = 0.05, overall.se = 0.02),
      ev = list(att.egt = c(0.01, 0.03, 0.05), se.egt = c(0.02, 0.02, 0.03), egt = c(-2, -1, 0))
    )
  )

  mock_data <- data.table::data.table(
    treatment_year = rep(c(2005, 2010, NA), each = 10),
    treated = rep(c(1, 1, 0), each = 10),
    outcome_var = rnorm(30)
  )

  agg <- list()
  ev <- list()

  result <- store_results(
    results = mock_results,
    data = mock_data,
    aggregate_results = agg,
    event_study_results = ev,
    pta_type = "cs",
    enforce_type = NULL,
    analysis_level = "county",
    outcome = "outcome_var",
    use_controls = NULL,
    drop_add_states = "none",
    result_type = "main",
    transform_outcome = NULL,
    treat_col = "treated",
    group_col = "treatment_year"
  )

  expect_length(result$aggregate_results, 1)
  expect_length(result$event_study_results, 1)
  expect_true("att" %in% names(result$aggregate_results[[1]]))
  expect_true("ybar" %in% names(result$aggregate_results[[1]]))
})

test_that("store_results backward compat with default column names", {
  mock_results <- list(
    imputation = list(
      agg = list(estimate = 0.03, std.error = 0.01),
      ev = list(estimate = c(0.01, 0.03), std.error = c(0.01, 0.02), term = c(-1, 0))
    )
  )

  mock_data <- data.table::data.table(
    year_passed = rep(c(2005, NA), each = 10),
    law_pass = rep(c(1, 0), each = 10),
    outcome_var = rnorm(20)
  )

  result <- store_results(
    results = mock_results,
    data = mock_data,
    aggregate_results = list(),
    event_study_results = list(),
    pta_type = "imputation",
    enforce_type = NULL,
    analysis_level = "county",
    outcome = "outcome_var",
    use_controls = NULL,
    drop_add_states = "none",
    result_type = "main",
    transform_outcome = NULL
  )

  expect_length(result$aggregate_results, 1)
})
