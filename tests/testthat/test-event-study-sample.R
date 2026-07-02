test_that("build_event_study_sample keeps clean units, drops interior-gap units", {
  library(data.table)
  # A: clean coverage 2004-2016 (adopts 2010) -> all of -3..3 present
  # B: "Nebraska" case -- wide envelope (2004-06, 2010, 2015-16) but missing the
  #    adjacent event times (-3..-1, 1..3)
  # C: never-treated
  d <- rbind(
    data.table(id = 1L, year = 2004:2016, g = 2010L),
    data.table(id = 2L, year = c(2004:2006, 2010L, 2015:2016), g = 2010L),
    data.table(id = 3L, year = 2004:2016, g = 0L)
  )
  es <- build_event_study_sample(d, "id", "year", "g", L = 3, K = 3)

  # support table verdicts
  expect_true(es$support[id == 1L, has_all_required])
  expect_false(es$support[id == 2L, has_all_required])
  expect_identical(es$support[id == 2L, missing_required], "-3, -2, -1, 1, 2, 3")

  # df_es keeps clean treated + never-treated, drops the gappy treated unit
  kept <- sort(unique(es$df_es$id))
  expect_identical(kept, c(1L, 3L))
})

test_that("build_event_study_sample treats NA group as never-treated", {
  library(data.table)
  d <- rbind(
    data.table(id = 1L, year = 2004:2016, g = 2010L),
    data.table(id = 2L, year = 2004:2016, g = NA_integer_)
  )
  es <- build_event_study_sample(d, "id", "year", "g", L = 3, K = 3)
  # NA-group unit is retained (never-treated), not in the support (treated) table
  expect_true(2L %in% es$df_es$id)
  expect_false(2L %in% es$support$id)
})
