# The CS control-counterfactual (.cs_delta_control) runs a did::att_gt fit that is
# invariant across simulations, effect sizes and noise configs. It must be memoized
# so the power grid does not repeat the identical fit thousands of times.

mk_memo_panel <- function() {
  library(data.table)
  set.seed(3)
  d <- CJ(id = 1:24, year = 2000:2012)
  # never-treated coded as numeric 0 (so did recognizes it); two in-window cohorts
  d[, g := c(0, 2005, 2008)[((id - 1L) %% 3L) + 1L]]
  d[, y := stats::rnorm(1, 10, 3), by = id][, y := y + 0.3 * (year - 2000) + stats::rnorm(.N, 0, 1)]
  d[]
}

test_that(".cs_delta_control memoizes identical fits and does not collide across inputs", {
  skip_if_not_installed("did")
  d <- mk_memo_panel()
  cache <- staggeredpower:::.sp_cs_delta_cache
  staggeredpower:::.sp_clear_cs_cache()
  expect_equal(length(ls(cache)), 0L)

  r1 <- staggeredpower:::.cs_delta_control(d, "id", "g", "year", "y", NULL,
                                           "notyettreated", "dr", "universal", FALSE)
  expect_equal(length(ls(cache)), 1L)                      # first call populated cache

  r2 <- staggeredpower:::.cs_delta_control(d, "id", "g", "year", "y", NULL,
                                           "notyettreated", "dr", "universal", FALSE)
  expect_equal(length(ls(cache)), 1L)                      # identical inputs -> cache hit
  expect_equal(r1, r2)

  # Different setting -> distinct cache entry, not a false hit
  r3 <- staggeredpower:::.cs_delta_control(d, "id", "g", "year", "y", NULL,
                                           "notyettreated", "reg", "universal", FALSE)
  expect_equal(length(ls(cache)), 2L)

  # Different outcome VALUES -> distinct entry
  d2 <- data.table::copy(d); d2[, y := y + 1]
  staggeredpower:::.cs_delta_control(d2, "id", "g", "year", "y", NULL,
                                     "notyettreated", "dr", "universal", FALSE)
  expect_equal(length(ls(cache)), 3L)
})

test_that("memoized result equals a fresh (uncached) computation", {
  skip_if_not_installed("did")
  d <- mk_memo_panel()
  staggeredpower:::.sp_clear_cs_cache()
  cached <- staggeredpower:::.cs_delta_control(d, "id", "g", "year", "y", NULL,
                                               "notyettreated", "dr", "universal", FALSE)
  staggeredpower:::.sp_clear_cs_cache()
  fresh <- staggeredpower:::.cs_delta_control(d, "id", "g", "year", "y", NULL,
                                              "notyettreated", "dr", "universal", FALSE)
  expect_equal(cached, fresh)
})
