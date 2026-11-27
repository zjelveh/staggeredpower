# tests/testthat/test-bootstrap-row-order.R
# The cluster bootstraps draw one weight (wcr_test) or one resampled cluster (the imputation adapter's
# staggeredpower.imp_boot option) per cluster. Which cluster gets which draw must not depend on the order of the
# rows: the same panel and seed in another row order must give the same result.
library(data.table)

make_order_panel <- function(n_units = 12, years = 2001:2010, seed = 3) {
  set.seed(seed)
  d <- CJ(id = seq_len(n_units), year = years)
  d[, g0 := rep(c(2005, 2007, 0), length.out = n_units)[id]]
  d[, treat_ind := as.integer(g0 > 0 & year >= g0)]
  d[, y := 0.5 * id + 0.2 * (year - 2000) + 0.5 * treat_ind + rnorm(.N)]
  d
}

shuffle_rows <- function(d, seed = 11) {
  set.seed(seed)
  s <- d[sample(.N)]
  stopifnot(!identical(unique(s$id), unique(d$id)))   # the clusters now first appear in another order
  s
}

wcr_on <- function(d, estimator, B) {
  suppressWarnings(wcr_test(d, outcome = "y", unit_var = "id", time_var = "year", group_var = "g0",
                            treat_ind_var = "treat_ind", estimator = estimator, B = B, seed = 1))
}

test_that("wcr_test gives the same result whatever the row order (twfe)", {
  skip_if_not_installed("fixest")
  d <- make_order_panel()
  s <- shuffle_rows(d)   # built before the call: a lazy argument would draw after wcr_test sets its seed
  a <- wcr_on(d, "twfe", 99L); b <- wcr_on(s, "twfe", 99L)
  expect_equal(b$estimate, a$estimate, tolerance = 1e-10)
  expect_equal(b$wcr_pval, a$wcr_pval)
  expect_equal(b$wcr_cval, a$wcr_cval, tolerance = 1e-8)
})

test_that("wcr_test gives the same result whatever the row order (imputation)", {
  skip_if_not_installed("didimputation")
  skip_if_not_installed("fixest")
  d <- make_order_panel()
  s <- shuffle_rows(d)
  a <- wcr_on(d, "imputation", 49L); b <- wcr_on(s, "imputation", 49L)
  expect_equal(b$estimate, a$estimate, tolerance = 1e-10)
  expect_equal(b$wcr_pval, a$wcr_pval)
  expect_equal(b$wcr_cval, a$wcr_cval, tolerance = 1e-8)
})

test_that("the imputation adapter's cluster bootstrap SE does not depend on the row order", {
  skip_if_not_installed("didimputation")
  d <- make_order_panel(n_units = 18)
  old <- options(staggeredpower.imp_boot = 12L); on.exit(options(old), add = TRUE)
  fit <- function(x) {
    set.seed(5)
    adapter_imputation()$fit(data = x, outcome_var = "y", time_var = "year", id_var = "id",
                             group_var = "g0", controls = NULL, cluster_var = "id")
  }
  s <- shuffle_rows(d)
  a <- fit(d); b <- fit(s)
  expect_equal(b$std.error, a$std.error, tolerance = 1e-8)
})
