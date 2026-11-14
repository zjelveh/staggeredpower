test_that("latent_v with hold_level centres each unit's path on zero", {
  dt <- data.table::CJ(unit = 1:6, time = 1:12)
  set.seed(3)
  latent_v(dt, "unit", "time", 0.9, hold_level = TRUE)
  expect_true(all(abs(dt[, mean(.v), by = unit]$V1) < 1e-12))
  expect_gt(sd(dt$.v), 0)
})

test_that("latent_v without hold_level draws what the code before the option drew", {
  for (rho in c(0, 0.6)) {
    dt <- data.table::CJ(unit = 1:5, time = 1:9)
    set.seed(11)
    latent_v(dt, "unit", "time", rho, hold_level = FALSE)
    ref <- data.table::CJ(unit = 1:5, time = 1:9)
    set.seed(11)
    if (rho == 0) {
      ref[, .v := stats::rnorm(.N, 0, 1)]
    } else {
      data.table::setorderv(ref, c("unit", "time"))
      ref[, .v := {
        n <- .N; z <- numeric(n); z[1] <- stats::rnorm(1, 0, 1)
        if (n > 1) { e <- stats::rnorm(n, 0, sqrt(1 - rho^2))
                    for (j in 2:n) z[j] <- rho * z[j - 1] + e[j] }
        z
      }, by = "unit"]
    }
    expect_identical(dt$.v, ref$.v)
  }
})

test_that("hold_level outside the Poisson re-roll model is an error, not ignored", {
  dt <- data.table::CJ(unit = 1:8, year = 2001:2010)
  dt[, `:=`(g = data.table::fifelse(unit <= 4, 2006L, 0L), pop = 1e5, y = 5)]
  dt[, treat := as.integer(g > 0 & year >= g)]
  expect_error(
    run_power_analysis(data_clean = dt, unit_var = "unit", time_var = "year", treat_ind_var = "treat",
                       group_var = "g", outcome = "y", pta_type = "imputation", percent_effect = 1,
                       models_to_run = "imputation", n_sims = 1, pop_var = "pop",
                       noise_spec = list(engine = "iid", obs_model = "gaussian", hold_level = TRUE)),
    "hold_level is implemented only")
})
