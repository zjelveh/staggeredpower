# tests/testthat/test-known-dgp.R
# Tests with KNOWN data-generating processes to verify estimators recover true effects.
# Uses deterministic or near-deterministic DGPs so failures indicate bugs, not randomness.

# ============================================================================
# HELPER: Create deterministic additive DGP
# y_{it} = alpha_i + delta_t + tau * D_{it}
# ============================================================================
make_additive_dgp <- function(n_units = 90,
                              n_periods = 15,
                              cohorts = c(2005, 2008),  # treatment years
                              n_per_cohort = c(30, 30), # treated units per cohort
                              tau = 3.0,                # true constant ATT
                              start_year = 2000,
                              noise_sd = 0.001) {
  set.seed(42)
  years <- start_year:(start_year + n_periods - 1)
  n_treated <- sum(n_per_cohort)
  n_control <- n_units - n_treated

  # Assign cohorts
  cohort_vec <- c(
    rep(cohorts[1], n_per_cohort[1]),
    rep(cohorts[2], n_per_cohort[2]),
    rep(0, n_control)  # never-treated coded as 0

  )

  # Unit fixed effects (heterogeneous levels)
  unit_fe <- seq(5, 15, length.out = n_units)

  # Time fixed effects (common linear trend)
  time_fe <- 0.5 * (years - start_year)

  # Build panel
  dt <- data.table::CJ(unit_id = 1:n_units, year = years)
  dt[, cohort := cohort_vec[unit_id]]
  dt[, post := as.integer(cohort > 0 & year >= cohort)]

  # Outcome: alpha_i + delta_t + tau * post + noise
  dt[, y := unit_fe[unit_id] + time_fe[year - start_year + 1] + tau * post +
       rnorm(.N, 0, noise_sd)]

  dt
}

# ============================================================================
# HELPER: Create deterministic Poisson (multiplicative) DGP
# E[count_{it}] = pop_i * exp(alpha_i + delta_t + tau * D_{it})
# We use deterministic counts (= expected value) so fixest recovers tau exactly.
# ============================================================================
make_poisson_dgp <- function(n_units = 90,
                             n_periods = 15,
                             cohorts = c(2005, 2008),
                             n_per_cohort = c(30, 30),
                             tau = log(1.2),           # true log ATT (20% increase)
                             start_year = 2000,
                             base_rate = 50) {         # per 100k
  set.seed(42)
  years <- start_year:(start_year + n_periods - 1)
  n_treated <- sum(n_per_cohort)
  n_control <- n_units - n_treated

  # Assign cohorts
  cohort_vec <- c(
    rep(cohorts[1], n_per_cohort[1]),
    rep(cohorts[2], n_per_cohort[2]),
    rep(0, n_control)
  )

  # Population per unit (varies, all large enough for Poisson to be well-behaved)
  pop_vec <- seq(200000, 500000, length.out = n_units)

  # Unit effects on log scale
  alpha_vec <- log(base_rate / 1e5) + seq(-0.3, 0.3, length.out = n_units)

  # Time effects on log scale (mild trend)
  delta_vec <- 0.02 * (years - start_year)

  # Build panel
  dt <- data.table::CJ(unit_id = 1:n_units, year = years)
  dt[, cohort := cohort_vec[unit_id]]
  dt[, population := pop_vec[unit_id]]
  dt[, post := as.integer(cohort > 0 & year >= cohort)]

  # Expected count: pop * exp(alpha + delta + tau*post)
  dt[, expected_rate := exp(alpha_vec[unit_id] + delta_vec[year - start_year + 1] + tau * post)]
  dt[, count := round(population * expected_rate)]

  # Also store rate per 100k for CS/imputation

  dt[, rate := count / population * 1e5]

  dt
}


# ============================================================================
# TEST 1: CS recovers true additive ATT
# ============================================================================
test_that("CS adapter recovers known additive ATT (tau = 3.0)", {
  skip_if_not_installed("did")

  tau_true <- 3.0
  dt <- make_additive_dgp(tau = tau_true)

  result <- estimate_models(
    data = dt,
    id_var = "unit_id",
    outcome_var = "y",
    time_var = "year",
    group_var = "cohort",
    models_to_run = "cs",
    cluster_var = "unit_id",
    n_cores = 1
  )

  expect_s3_class(result$cs, "standard_estimate")
  att_hat <- result$cs$agg$att

  # With near-zero noise, CS should recover tau almost exactly

  expect_true(abs(att_hat - tau_true) < 0.1,
              info = sprintf("CS ATT = %.4f, expected %.4f", att_hat, tau_true))
})

# ============================================================================
# TEST 2: Imputation adapter recovers true additive ATT
# ============================================================================
test_that("Imputation adapter recovers known additive ATT (tau = 3.0)", {
  skip_if_not_installed("didimputation")

  tau_true <- 3.0
  dt <- make_additive_dgp(tau = tau_true)

  result <- estimate_models(
    data = dt,
    id_var = "unit_id",
    outcome_var = "y",
    time_var = "year",
    group_var = "cohort",
    models_to_run = "imputation",
    cluster_var = "unit_id",
    n_cores = 1
  )

  expect_s3_class(result$imputation, "standard_estimate")
  att_hat <- result$imputation$agg$att

  expect_true(abs(att_hat - tau_true) < 0.1,
              info = sprintf("Imputation ATT = %.4f, expected %.4f", att_hat, tau_true))
})

# ============================================================================
# TEST 3: ETWFE Poisson recovers true log ATT
# ============================================================================
test_that("ETWFE Poisson recovers known log ATT (tau = log(1.2))", {
  skip_if_not_installed("fixest")

  tau_true <- log(1.2)
  dt <- make_poisson_dgp(tau = tau_true)

  result <- estimate_models(
    data = dt,
    id_var = "unit_id",
    outcome_var = "count",
    time_var = "year",
    group_var = "cohort",
    models_to_run = "etwfe_poisson",
    cluster_var = "unit_id",
    outcome_type = "count",
    pop_var = "population",
    n_cores = 1
  )

  expect_s3_class(result$etwfe_poisson, "standard_estimate")
  att_hat <- result$etwfe_poisson$agg$att

  # Poisson MLE on deterministic data should be very close to true tau
  expect_true(abs(att_hat - tau_true) < 0.02,
              info = sprintf("ETWFE Poisson ATT = %.4f, expected %.4f (log(1.2) = %.4f)",
                             att_hat, tau_true, tau_true))
})

# ============================================================================
# TEST 4: CS + imputation on count data via outcome_type="count" pathway
# This tests the count→rate conversion inside the adapters.
# ============================================================================
test_that("CS adapter correctly converts count to rate (outcome_type='count')", {
  skip_if_not_installed("did")

  # Use a Poisson DGP but with known additive rate effect
  # rate = base_rate + tau_rate * post
  # This is NOT the log-linear DGP — it's additive in rate space
  set.seed(123)
  tau_rate <- 5.0  # 5 per 100k increase
  base_rate <- 50  # per 100k

  n_units <- 60
  n_periods <- 12
  years <- 2000:2011
  cohort_vec <- c(rep(2005, 20), rep(2008, 20), rep(0, 20))
  pop_vec <- rep(200000, n_units)
  unit_fe <- seq(-2, 2, length.out = n_units)

  dt <- data.table::CJ(unit_id = 1:n_units, year = years)
  dt[, cohort := cohort_vec[unit_id]]
  dt[, population := pop_vec[unit_id]]
  dt[, post := as.integer(cohort > 0 & year >= cohort)]

  # Rate is additive: base + unit_fe + tau*post
  dt[, rate := base_rate + unit_fe[unit_id] + tau_rate * post]
  # Count = rate * pop / 100k
  dt[, count := round(rate * population / 1e5)]

  # Run CS with outcome_type = "count" — adapter should convert to rate
  result <- estimate_models(
    data = dt,
    id_var = "unit_id",
    outcome_var = "count",
    time_var = "year",
    group_var = "cohort",
    models_to_run = "cs",
    cluster_var = "unit_id",
    outcome_type = "count",
    pop_var = "population",
    n_cores = 1
  )

  att_hat <- result$cs$agg$att

  # CS should recover tau_rate (in rate per 100k units)
  expect_true(abs(att_hat - tau_rate) < 0.5,
              info = sprintf("CS (count→rate) ATT = %.4f, expected %.4f", att_hat, tau_rate))
})

# ============================================================================
# TEST 5: CS event study shows zero pre-treatment and tau post-treatment
# ============================================================================
test_that("CS event study shows zero pre-treatment effects", {
  skip_if_not_installed("did")

  tau_true <- 3.0
  dt <- make_additive_dgp(tau = tau_true)

  result <- estimate_models(
    data = dt,
    id_var = "unit_id",
    outcome_var = "y",
    time_var = "year",
    group_var = "cohort",
    models_to_run = "cs",
    cluster_var = "unit_id",
    event_study = TRUE,
    n_cores = 1
  )

  es <- result$cs$event_study
  expect_s3_class(es, "data.table")
  expect_true(all(c("rel_time", "att", "se") %in% names(es)))

  # Pre-treatment effects should be ~0
  pre <- es[rel_time < 0]
  expect_true(nrow(pre) > 0, info = "Should have pre-treatment periods")
  expect_true(all(abs(pre$att) < 0.5),
              info = sprintf("Max pre-treatment effect = %.4f (should be ~0)", max(abs(pre$att))))

  # Post-treatment effects should be ~tau
  post <- es[rel_time >= 0]
  expect_true(nrow(post) > 0, info = "Should have post-treatment periods")
  expect_true(all(abs(post$att - tau_true) < 0.5),
              info = sprintf("Post-treatment effects should be ~%.1f", tau_true))
})

# ============================================================================
# TEST 6: Imputation event study shows zero pre-treatment and tau post-treatment
# ============================================================================
test_that("Imputation event study shows zero pre-treatment effects", {
  skip_if_not_installed("didimputation")

  tau_true <- 3.0
  dt <- make_additive_dgp(tau = tau_true)

  result <- estimate_models(
    data = dt,
    id_var = "unit_id",
    outcome_var = "y",
    time_var = "year",
    group_var = "cohort",
    models_to_run = "imputation",
    cluster_var = "unit_id",
    event_study = TRUE,
    n_cores = 1
  )

  es <- result$imputation$event_study
  expect_s3_class(es, "data.table")
  expect_true(all(c("rel_time", "att", "se") %in% names(es)))

  # Pre-treatment effects should be ~0
  pre <- es[rel_time < 0]
  expect_true(nrow(pre) > 0, info = "Should have pre-treatment periods")
  expect_true(all(abs(pre$att) < 0.5),
              info = sprintf("Max pre-treatment effect = %.4f (should be ~0)", max(abs(pre$att))))

  # Post-treatment effects should be ~tau
  post <- es[rel_time >= 0]
  expect_true(nrow(post) > 0, info = "Should have post-treatment periods")
  expect_true(all(abs(post$att - tau_true) < 0.5),
              info = sprintf("Post-treatment effects should be ~%.1f", tau_true))
})

# ============================================================================
# TEST 7: ETWFE Poisson event study shows zero pre-treatment leads and tau post
# ============================================================================
test_that("ETWFE Poisson event study shows zero pre-treatment and tau post-treatment", {
  skip_if_not_installed("fixest")

  tau_true <- log(1.2)
  dt <- make_poisson_dgp(tau = tau_true)

  result <- estimate_models(
    data = dt,
    id_var = "unit_id",
    outcome_var = "count",
    time_var = "year",
    group_var = "cohort",
    models_to_run = "etwfe_poisson",
    cluster_var = "unit_id",
    outcome_type = "count",
    pop_var = "population",
    event_study = TRUE,
    n_cores = 1
  )

  es <- result$etwfe_poisson$event_study
  expect_s3_class(es, "data.table")
  expect_true(all(c("rel_time", "att", "se") %in% names(es)))

  # Pre-treatment leads should be ~0 (no treatment effect before treatment)
  pre <- es[rel_time < 0]
  expect_true(nrow(pre) > 0, info = "Should have pre-treatment leads (e < -1)")
  expect_true(all(abs(pre$att) < 0.05),
              info = sprintf("Max pre-treatment lead = %.4f (should be ~0)", max(abs(pre$att))))

  # Post-treatment effects should be ~tau on log scale
  post <- es[rel_time >= 0]
  expect_true(nrow(post) > 0, info = "Should have post-treatment periods")
  expect_true(all(abs(post$att - tau_true) < 0.05),
              info = sprintf("Post-treatment effects should be ~%.4f (log(1.2))", tau_true))

  # Reference period e = -1 should NOT appear
  expect_false(-1 %in% es$rel_time,
               info = "Reference period (e=-1) should be excluded from event study")
})

# ============================================================================
# TEST 8: ETWFE Poisson ATT consistent between event_study=TRUE and FALSE
# When event_study=TRUE, the model includes pre-treatment leads which slightly
# changes the model (leads absorb some ref-group variation). But the overall
# ATT (aggregated from post-treatment cells only) should still be very close.
# ============================================================================
test_that("ETWFE Poisson overall ATT consistent with/without event_study", {
  skip_if_not_installed("fixest")

  tau_true <- log(1.2)
  dt <- make_poisson_dgp(tau = tau_true)

  result_no_es <- estimate_models(
    data = dt,
    id_var = "unit_id",
    outcome_var = "count",
    time_var = "year",
    group_var = "cohort",
    models_to_run = "etwfe_poisson",
    cluster_var = "unit_id",
    outcome_type = "count",
    pop_var = "population",
    event_study = FALSE,
    n_cores = 1
  )

  result_es <- estimate_models(
    data = dt,
    id_var = "unit_id",
    outcome_var = "count",
    time_var = "year",
    group_var = "cohort",
    models_to_run = "etwfe_poisson",
    cluster_var = "unit_id",
    outcome_type = "count",
    pop_var = "population",
    event_study = TRUE,
    n_cores = 1
  )

  att_no_es <- result_no_es$etwfe_poisson$agg$att
  att_es <- result_es$etwfe_poisson$agg$att

  # Both should be close to true tau
  expect_true(abs(att_no_es - tau_true) < 0.02,
              info = sprintf("No-ES ATT = %.4f", att_no_es))
  expect_true(abs(att_es - tau_true) < 0.02,
              info = sprintf("ES ATT = %.4f", att_es))

  # And close to each other
  expect_true(abs(att_no_es - att_es) < 0.01,
              info = sprintf("Difference = %.6f", abs(att_no_es - att_es)))
})

# ============================================================================
# TEST 9: All three estimators on the SAME data (count DGP)
# CS/imputation operate on rate, ETWFE on log-count.
# They should all detect a significant effect in the correct direction.
# ============================================================================
test_that("All three estimators detect effect on same Poisson data", {
  skip_if_not_installed("did")
  skip_if_not_installed("didimputation")
  skip_if_not_installed("fixest")

  tau_true <- log(1.3)  # 30% increase (bigger effect for clear detection)
  dt <- make_poisson_dgp(tau = tau_true, base_rate = 50)

  # Run all three
  result <- estimate_models(
    data = dt,
    id_var = "unit_id",
    outcome_var = "count",
    time_var = "year",
    group_var = "cohort",
    models_to_run = c("cs", "imputation", "etwfe_poisson"),
    cluster_var = "unit_id",
    outcome_type = "count",
    pop_var = "population",
    n_cores = 1
  )

  # ETWFE Poisson: should recover log ATT directly
  etwfe_att <- result$etwfe_poisson$agg$att
  expect_true(abs(etwfe_att - tau_true) < 0.03,
              info = sprintf("ETWFE log ATT = %.4f vs true %.4f", etwfe_att, tau_true))

  # CS and imputation: operate on rate (per 100k).
  # Expected rate increase = base_rate * (exp(tau) - 1) ≈ 50 * 0.35 = 17.5
  # But exact value depends on weighted average across units/times.
  # Just check: positive, significant, and reasonable magnitude.
  cs_att <- result$cs$agg$att
  imp_att <- result$imputation$agg$att

  expect_true(cs_att > 0,
              info = sprintf("CS ATT = %.4f (should be positive)", cs_att))
  expect_true(imp_att > 0,
              info = sprintf("Imputation ATT = %.4f (should be positive)", imp_att))

  # With base_rate=50 and 30% increase, rate effect should be ~15-20 per 100k
  # Allow wider range due to heterogeneous baseline rates across units
  expect_true(cs_att > 5 & cs_att < 40,
              info = sprintf("CS rate ATT = %.2f (expected ~15-20)", cs_att))
  expect_true(imp_att > 5 & imp_att < 40,
              info = sprintf("Imputation rate ATT = %.2f (expected ~15-20)", imp_att))
})

# ============================================================================
# TEST 10: Zero treatment effect → all estimators return ATT ≈ 0
# ============================================================================
test_that("All estimators return ATT ≈ 0 when true effect is zero", {
  skip_if_not_installed("did")
  skip_if_not_installed("didimputation")
  skip_if_not_installed("fixest")

  # Additive DGP with tau = 0
  dt_add <- make_additive_dgp(tau = 0.0)

  result_add <- estimate_models(
    data = dt_add,
    id_var = "unit_id",
    outcome_var = "y",
    time_var = "year",
    group_var = "cohort",
    models_to_run = c("cs", "imputation"),
    cluster_var = "unit_id",
    n_cores = 1
  )

  expect_true(abs(result_add$cs$agg$att) < 0.1,
              info = sprintf("CS ATT = %.4f (should be ~0)", result_add$cs$agg$att))
  expect_true(abs(result_add$imputation$agg$att) < 0.1,
              info = sprintf("Imputation ATT = %.4f (should be ~0)", result_add$imputation$agg$att))

  # Poisson DGP with tau = 0 (no treatment effect)
  dt_pois <- make_poisson_dgp(tau = 0.0)

  result_pois <- estimate_models(
    data = dt_pois,
    id_var = "unit_id",
    outcome_var = "count",
    time_var = "year",
    group_var = "cohort",
    models_to_run = "etwfe_poisson",
    cluster_var = "unit_id",
    outcome_type = "count",
    pop_var = "population",
    n_cores = 1
  )

  expect_true(abs(result_pois$etwfe_poisson$agg$att) < 0.01,
              info = sprintf("ETWFE Poisson ATT = %.6f (should be ~0)",
                             result_pois$etwfe_poisson$agg$att))
})

# ============================================================================
# TEST 11: SEs are positive and reasonable (not zero, not huge)
# ============================================================================
test_that("Standard errors are positive and finite for known DGP", {
  skip_if_not_installed("did")
  skip_if_not_installed("didimputation")
  skip_if_not_installed("fixest")

  dt_add <- make_additive_dgp(tau = 3.0, noise_sd = 0.5)  # some noise for non-zero SEs
  dt_pois <- make_poisson_dgp(tau = log(1.2))

  result_add <- estimate_models(
    data = dt_add,
    id_var = "unit_id",
    outcome_var = "y",
    time_var = "year",
    group_var = "cohort",
    models_to_run = c("cs", "imputation"),
    cluster_var = "unit_id",
    n_cores = 1
  )

  result_pois <- estimate_models(
    data = dt_pois,
    id_var = "unit_id",
    outcome_var = "count",
    time_var = "year",
    group_var = "cohort",
    models_to_run = "etwfe_poisson",
    cluster_var = "unit_id",
    outcome_type = "count",
    pop_var = "population",
    n_cores = 1
  )

  # CS SE
  se_cs <- result_add$cs$agg$se
  expect_true(is.finite(se_cs) && se_cs > 0,
              info = sprintf("CS SE = %s", se_cs))

  # Imputation SE
  se_imp <- result_add$imputation$agg$se
  expect_true(is.finite(se_imp) && se_imp > 0,
              info = sprintf("Imputation SE = %s", se_imp))

  # ETWFE Poisson SE
  se_etw <- result_pois$etwfe_poisson$agg$se
  expect_true(is.finite(se_etw) && se_etw > 0,
              info = sprintf("ETWFE Poisson SE = %s", se_etw))

  # SEs should be much smaller than the effect itself (high signal-to-noise)
  expect_true(se_cs < 3.0,
              info = sprintf("CS SE = %.4f (should be << 3.0)", se_cs))
  expect_true(se_etw < 0.182,
              info = sprintf("ETWFE SE = %.4f (should be << log(1.2))", se_etw))
})

# ============================================================================
# TEST 12: Dynamic treatment effects (effect grows over time)
# ============================================================================
test_that("Event study captures dynamic treatment effects correctly", {
  skip_if_not_installed("did")

  # DGP with linearly increasing treatment effect: tau(e) = 1 + 0.5*e for e >= 0
  set.seed(99)
  n_units <- 60
  n_periods <- 12
  years <- 2000:2011
  cohort_vec <- c(rep(2005, 20), rep(2008, 20), rep(0, 20))
  unit_fe <- seq(5, 15, length.out = n_units)
  time_fe <- 0.3 * (years - 2000)

  dt <- data.table::CJ(unit_id = 1:n_units, year = years)
  dt[, cohort := cohort_vec[unit_id]]
  dt[, rel_time_true := ifelse(cohort > 0, year - cohort, NA_integer_)]
  dt[, tau_dynamic := ifelse(!is.na(rel_time_true) & rel_time_true >= 0,
                              1.0 + 0.5 * rel_time_true, 0)]
  dt[, y := unit_fe[unit_id] + time_fe[year - 2000 + 1] + tau_dynamic + rnorm(.N, 0, 0.01)]

  # Drop helper columns not needed by estimator
  dt[, c("rel_time_true", "tau_dynamic") := NULL]

  result <- estimate_models(
    data = dt,
    id_var = "unit_id",
    outcome_var = "y",
    time_var = "year",
    group_var = "cohort",
    models_to_run = "cs",
    cluster_var = "unit_id",
    event_study = TRUE,
    n_cores = 1
  )

  es <- result$cs$event_study
  post <- es[rel_time >= 0]

  # Check that effects are monotonically increasing
  if (nrow(post) >= 3) {
    # Effects at e=0,1,2 should be approximately 1.0, 1.5, 2.0
    e0 <- post[rel_time == 0, att]
    e1 <- post[rel_time == 1, att]
    e2 <- post[rel_time == 2, att]

    expect_true(abs(e0 - 1.0) < 0.3, info = sprintf("e=0: %.3f vs 1.0", e0))
    expect_true(abs(e1 - 1.5) < 0.3, info = sprintf("e=1: %.3f vs 1.5", e1))
    expect_true(abs(e2 - 2.0) < 0.3, info = sprintf("e=2: %.3f vs 2.0", e2))

    # Monotonicity check
    expect_true(e1 > e0, info = "Effect should increase: e=1 > e=0")
    expect_true(e2 > e1, info = "Effect should increase: e=2 > e=1")
  }
})

# ============================================================================
# TEST 13: ETWFE metadata contains expected fields
# ============================================================================
test_that("ETWFE Poisson metadata contains att_pct and related fields", {
  skip_if_not_installed("fixest")

  tau_true <- log(1.2)
  dt <- make_poisson_dgp(tau = tau_true)

  result <- estimate_models(
    data = dt,
    id_var = "unit_id",
    outcome_var = "count",
    time_var = "year",
    group_var = "cohort",
    models_to_run = "etwfe_poisson",
    cluster_var = "unit_id",
    outcome_type = "count",
    pop_var = "population",
    n_cores = 1
  )

  meta <- result$etwfe_poisson$metadata

  # Expected metadata fields
  expect_true("att_pct" %in% names(meta))
  expect_true("att_se_pct" %in% names(meta))
  expect_true("n_obs" %in% names(meta))
  expect_true("n_clusters" %in% names(meta))
  expect_true("n_treat_cells" %in% names(meta))
  expect_true("se_type" %in% names(meta))

  # att_pct should be exp(tau) - 1 ≈ 0.2 (20% increase)
  expect_true(abs(meta$att_pct - 0.2) < 0.03,
              info = sprintf("att_pct = %.4f vs expected 0.2", meta$att_pct))

  # n_obs should equal nrow of panel
  expect_equal(meta$n_obs, 90 * 15)

  # n_clusters should equal n_units
  expect_equal(meta$n_clusters, 90)
})

# ============================================================================
# TEST 14: Imputation event study matches CS event study closely
# Under the same additive DGP, both should give very similar event study estimates.
# ============================================================================
test_that("Imputation and CS event studies agree under additive DGP", {
  skip_if_not_installed("did")
  skip_if_not_installed("didimputation")

  tau_true <- 3.0
  dt <- make_additive_dgp(tau = tau_true)

  result <- estimate_models(
    data = dt,
    id_var = "unit_id",
    outcome_var = "y",
    time_var = "year",
    group_var = "cohort",
    models_to_run = c("cs", "imputation"),
    cluster_var = "unit_id",
    event_study = TRUE,
    n_cores = 1
  )

  es_cs <- result$cs$event_study
  es_imp <- result$imputation$event_study

  # Both should exist
  expect_s3_class(es_cs, "data.table")
  expect_s3_class(es_imp, "data.table")

  # Find common relative times
  common_times <- intersect(es_cs$rel_time, es_imp$rel_time)
  expect_true(length(common_times) > 5,
              info = "Should have many overlapping relative times")

  # Compare effects at common times
  for (e in common_times) {
    att_cs <- es_cs[rel_time == e, att]
    att_imp <- es_imp[rel_time == e, att]
    # Under additive DGP with minimal noise, both should be very similar
    expect_true(abs(att_cs - att_imp) < 0.5,
                info = sprintf("e=%d: CS=%.3f, Imputation=%.3f, diff=%.3f",
                               e, att_cs, att_imp, abs(att_cs - att_imp)))
  }
})
