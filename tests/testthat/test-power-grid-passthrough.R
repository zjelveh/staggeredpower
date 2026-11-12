# tests/testthat/test-power-grid-passthrough.R
# run_power_grid() must forward pop_var (and other run_power_analysis() arguments) instead of
# silently dropping them; run_power_analysis() must fail clearly on a panel with no treated cells.

library(data.table)

make_grid_test_panel <- function(treated = TRUE) {
  set.seed(7)
  dt <- CJ(unit = 1:12, year = 2000:2011)
  dt[, pop := 1e5]
  dt[, year_passed := if (treated) fifelse(unit <= 4, 2005L, fifelse(unit <= 8, 2008L, NA_integer_)) else NA_integer_]
  dt[, treat_ind := fifelse(!is.na(year_passed) & year >= year_passed, 1L, 0L)]
  dt[, rel_pass := fifelse(!is.na(year_passed), year - year_passed, NA_integer_)]
  dt[, y := rpois(.N, 5)]
  dt
}

fake_power_result <- function(...) {
  list(final_power = data.table(model = "cs", pta_type = "cs", enforce_type = "no_controls",
                                controls = "no_controls", percent_effect = 0.9,
                                att = -0.5, se = 0.2, n_violating_units = 0L, sim = 1L))
}

test_that("run_power_grid forwards pop_var to run_power_analysis", {
  seen <- new.env()
  local_mocked_bindings(run_power_analysis = function(...) {
    seen$args <- list(...)
    fake_power_result()
  })
  run_power_grid(make_grid_test_panel(), unit_var = "unit", group_var = "year_passed",
                 time_var = "year", rel_pass_var = "rel_pass", treat_ind_var = "treat_ind",
                 outcome = "y", pta_type = "cs", percent_effect = 0.9, models_to_run = "cs",
                 n_sims = 1, pop_var = "pop")
  expect_identical(seen$args$pop_var, "pop")
})

test_that("run_power_grid passes other run_power_analysis arguments through ...", {
  seen <- new.env()
  local_mocked_bindings(run_power_analysis = function(...) {
    seen$args <- list(...)
    fake_power_result()
  })
  run_power_grid(make_grid_test_panel(), unit_var = "unit", group_var = "year_passed",
                 time_var = "year", rel_pass_var = "rel_pass", treat_ind_var = "treat_ind",
                 outcome = "y", pta_type = "cs", percent_effect = 0.9, models_to_run = "cs",
                 n_sims = 1, outcome_type = "rate", allow_unbalanced_panel = TRUE, est_method = "reg")
  expect_identical(seen$args$outcome_type, "rate")
  expect_true(seen$args$allow_unbalanced_panel)
  expect_identical(seen$args$est_method, "reg")
})

test_that("run_power_analysis errors clearly when the panel has no treated cells", {
  skip_if_not_installed("did")
  expect_error(
    run_power_analysis(make_grid_test_panel(treated = FALSE), unit_var = "unit",
                       group_var = "year_passed", time_var = "year", rel_pass_var = "rel_pass",
                       treat_ind_var = "treat_ind", outcome = "y", pta_type = "cs",
                       percent_effect = 0.9, models_to_run = "cs", n_sims = 1),
    "no treated"
  )
})
