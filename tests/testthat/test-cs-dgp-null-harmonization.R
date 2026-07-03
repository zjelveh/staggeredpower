# The CS power DGP (enforce_PTA_CS) must be HARMONIZED with the CS estimator
# (did::att_gt): when the DGP builds a null counterfactual (percent_effect = 1.0)
# and the same CS estimator is applied to it, the recovered ATT must be ~0 on ANY
# panel -- balanced OR unbalanced. Historically this held on balanced panels but
# NOT on ragged ones, because the DGP re-derived did's control counterfactual with
# hand-rolled code that only matched did on complete-grid panels.

make_ragged_panel <- function() {
  library(data.table)
  set.seed(11)
  years <- 2000:2015
  n <- 48L
  # NA = never-treated; 2019 = late adopter (treated AFTER the window -> a clean
  # not-yet-treated control that did keeps but the old DGP dropped); rest in-window.
  cohort <- c(NA, NA, 2019L, 2006L, 2009L, 2012L)
  d <- CJ(id = 1:n, year = years)
  d[, g := cohort[((id - 1L) %% length(cohort)) + 1L]]
  maxy <- max(years)
  d[, treat_ind := as.integer(!is.na(g) & g <= maxy & year >= g)]
  d[, rel_pass  := ifelse(!is.na(g), year - g, NA_integer_)]
  d[, unit_fe := stats::rnorm(1, 50, 10), by = id]
  d[, time_fe := 0.4 * (year - 2000L) + stats::rnorm(1, 0, 1), by = year]
  d[, y := unit_fe + time_fe + treat_ind * 4 + stats::rnorm(.N, 0, 2)]
  # Unbalanced via CONTROL-side raggedness only: drop 3 scattered years from each
  # never-treated / late-adopter unit, leaving treated cohorts fully observed. The
  # (id x year) grid is incomplete (unbalanced), but the control POOL composition is
  # what varies -- exactly the NIBRS structure that surfaced the bug.
  ctrl_ids <- unique(d[is.na(g), id])
  drop <- d[id %in% ctrl_ids, .(year = sample(year, 3L)), by = id]
  d <- d[!drop, on = .(id, year)]
  d[]
}

test_that("CS DGP recovers the null (ATT ~ 0) on an unbalanced panel", {
  skip_if_not_installed("did")
  d <- make_ragged_panel()

  # sanity: the panel really is unbalanced (incomplete id x year grid)
  expect_lt(nrow(unique(d[, .(id, year)])),
            data.table::uniqueN(d$id) * data.table::uniqueN(d$year))

  res <- run_power_analysis(
    data_clean = d, unit_var = "id", group_var = "g", time_var = "year",
    rel_pass_var = "rel_pass", treat_ind_var = "treat_ind",
    controls = NULL, outcome = "y", pta_type = "cs",
    percent_effect = 1.0,               # NULL: no injected effect
    models_to_run = "cs", n_sims = 1L,
    noise_spec = list(engine = "none"), # deterministic benchmark
    allow_unbalanced_panel = TRUE, est_method = "dr", base_period = "universal"
  )

  att_null <- res$final_power[model == "cs"]$att
  expect_length(att_null, 1L)
  expect_lt(abs(att_null), 1e-6)        # harmonized DGP -> null recovers to ~0
})

test_that("a cohort with no observed post-period does not corrupt pre-period data or crash", {
  skip_if_not_installed("did")
  library(data.table)
  set.seed(5)
  d <- CJ(id = 1:30, year = 2000:2012)
  # cohorts 2004/2008 (normal, in-window); 2013 = adopts AFTER the last observed year
  # (2012) -> those units are untreated in-window with max(rel_pass) = -1.
  d[, g := c(2004L, 2008L, 2013L)[((id - 1L) %% 3L) + 1L]]
  d[, treat_ind := as.integer(g <= 2012L & year >= g)]
  d[, rel_pass := year - g]
  d[, y := stats::rnorm(1, 20, 5), by = id][, y := y + 0.5 * (year - 2000) + stats::rnorm(.N, 0, 2)]

  y_before <- copy(d$y)
  cf <- expect_no_error(
    staggeredpower:::enforce_PTA_CS(copy(d), "id", "g", "year", "y", controls = NULL,
      noise_spec = list(engine = "none"), control_group = "notyettreated",
      est_method = "dr", base_period = "universal", allow_unbalanced_panel = TRUE)
  )
  setDT(cf)
  # The 2013 cohort has NO post-period in-window; its rows (all pre-treatment) must be
  # left untouched -- never overwritten by a negative-rp iteration.
  c2013 <- cf[g == 2013L]
  expect_equal(c2013$counterfactual, c2013$y)
})
