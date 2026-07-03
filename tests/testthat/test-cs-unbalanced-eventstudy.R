# Tests for the 0.2.0 CS-adapter additions: allow_unbalanced_panel exposure +
# warning, and the balance_e/min_e/max_e event-study aggregation controls.

make_unbalanced <- function() {
  library(data.table)
  set.seed(123)
  n_units <- 40L; years <- 2004:2018
  d <- CJ(state = 1:n_units, year = years)
  cohort <- rep(c(0L, 2010L, 2013L), length.out = n_units)   # multi-cohort
  d[, g := cohort[state]]
  d[, treated := as.integer(g > 0 & year >= g)]
  d[, unit_fe := rnorm(1, 50, 10), by = state]
  d[, time_fe := rnorm(1, 0, 2), by = year]
  d[, y := unit_fe + time_fe + treated * 5 + rnorm(.N, 0, 3)]
  d[, id := state]
  # Make it unbalanced: drop a couple of interior years from one treated unit
  d[!(state == 1L & year %in% c(2008L, 2009L))]
}

test_that("CS adapter warns on unbalanced panel with allow_unbalanced_panel = FALSE", {
  skip_if_not_installed("did")
  d <- make_unbalanced()
  # Collect all warnings; assert OUR anti-footgun warning is among them.
  msgs <- character(0)
  withCallingHandlers(
    tryCatch(estimate_models(d, "id", "y", "year", "g", models_to_run = "cs"),
             error = function(e) NULL),
    warning = function(cond) { msgs <<- c(msgs, conditionMessage(cond)); invokeRestart("muffleWarning") }
  )
  expect_true(any(grepl("unbalanced", msgs) & grepl("allow_unbalanced_panel", msgs)))
})

test_that("allow_unbalanced_panel = TRUE changes the estimand vs the coerced FALSE run", {
  skip_if_not_installed("did")
  library(data.table)
  d <- make_unbalanced()
  # Make MANY treated units unbalanced so the balanced coercion (FALSE) drops a
  # meaningful chunk -> the two estimands should differ.
  treated_ids <- unique(d[g > 0, id])
  drop_ids <- treated_ids[seq_len(floor(length(treated_ids) * 0.6))]
  d <- d[!(id %in% drop_ids & year %in% c(2008L, 2009L, 2011L))]
  att_false <- suppressWarnings(estimate_models(d, "id", "y", "year", "g",
                                                models_to_run = "cs")$cs$agg$att)
  att_true  <- suppressWarnings(estimate_models(d, "id", "y", "year", "g",
                                                models_to_run = "cs",
                                                allow_unbalanced_panel = TRUE)$cs$agg$att)
  expect_true(is.finite(att_true))
  expect_false(isTRUE(all.equal(att_false, att_true)))   # different estimands
})

test_that("max_e controls the event-study post window", {
  skip_if_not_installed("did")
  d <- make_unbalanced()
  narrow <- suppressWarnings(estimate_models(d, "id", "y", "year", "g",
                models_to_run = "cs", event_study = TRUE,
                allow_unbalanced_panel = TRUE, min_e = -3, max_e = 2)$cs$event_study)
  wide   <- suppressWarnings(estimate_models(d, "id", "y", "year", "g",
                models_to_run = "cs", event_study = TRUE,
                allow_unbalanced_panel = TRUE, min_e = -3, max_e = 5)$cs$event_study)
  # Bounds are respected, and a larger max_e yields a wider (or equal) window.
  expect_lte(max(narrow$rel_time), 2)
  expect_true(all(narrow$rel_time >= -3))
  expect_gte(max(wide$rel_time), max(narrow$rel_time))
})
