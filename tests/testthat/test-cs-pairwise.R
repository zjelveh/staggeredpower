# cs_unbalanced = "pairwise": Callaway-Sant'Anna on an unbalanced panel, with each
# ATT(g, t) estimated on the units observed in both of its years (did's panel 2x2
# estimator), in place of did's repeated-cross-section estimator ("rc"), whose state
# composition differs between the two years.

make_gappy <- function(seed = 1, noise = 0, balanced = FALSE) {
  library(data.table)
  set.seed(seed)
  years <- 2000:2015; n <- 40L
  cohort <- rep(c(NA, 2005L, 2008L, 2011L, 2014L), length.out = n)   # 8 never-treated
  d <- CJ(id = seq_len(n), year = years)
  d[, g := cohort[id]]
  d[, a := stats::rnorm(1, 50, 10), by = id]
  d[, b := 0.5 * (year - 2000) + stats::rnorm(1, 0, 1), by = year]
  d[, x := stats::rnorm(.N)]                                           # time-varying covariate
  d[, y := a + b + noise * stats::rnorm(.N)]                           # additive: no effect, parallel trends
  d[, treat_ind := as.integer(!is.na(g) & year >= g)]
  d[, rel_pass := ifelse(is.na(g), NA_integer_, year - g)]
  if (!balanced) {
    hi <- d[, .(a = a[1]), by = id][a > stats::quantile(a, 2 / 3), id]
    d <- d[!(id %in% hi & year <= 2003)]                  # gaps that depend on the level
    d <- d[!(id == 2L & year == 2004L)]                   # cohort 2005 unit missing its base year
    d <- d[!(id == 3L & year %in% c(2009L, 2010L))]       # cohort 2008 unit missing post years
    d <- d[!(id == 1L & year %in% c(2007L, 2012L))]       # never-treated unit, interior gaps
  }
  d[, g0 := ifelse(is.na(g), 0, as.numeric(g))]
  d[]
}

cs_fit <- function(d, ..., y = "y", seed = 1) {
  set.seed(seed)
  suppressWarnings(suppressMessages(estimate_models(
    d, "id", y, "year", "g0", models_to_run = "cs", n_cores = 1,
    allow_unbalanced_panel = TRUE, base_period = "universal", ...)))$cs
}

did_panel_ref <- function(d, xformla, est_method, seed = 1) {
  set.seed(seed)
  m <- suppressWarnings(suppressMessages(did::att_gt(
    yname = "y", tname = "year", idname = "id", gname = "g0", xformla = xformla,
    data = as.data.frame(d), panel = TRUE, allow_unbalanced_panel = FALSE,
    control_group = "notyettreated", anticipation = 0, est_method = est_method,
    clustervars = "id", base_period = "universal", print_details = FALSE)))
  s <- did::aggte(m, type = "simple", na.rm = TRUE)
  e <- did::aggte(m, type = "dynamic", na.rm = TRUE)
  list(m = m, s = s, e = e)
}

expect_did_panel_equal <- function(r, ref) {
  expect_identical(r$metadata$cs_unbalanced, "pairwise")
  expect_equal(r$raw$group, ref$m$group)
  expect_equal(r$raw$t, ref$m$t)
  expect_equal(r$raw$att, ref$m$att, tolerance = 1e-10)
  expect_equal(as.matrix(r$raw$inffunc), as.matrix(ref$m$inffunc), tolerance = 1e-10)
  expect_equal(r$agg$att, ref$s$overall.att, tolerance = 1e-10)
  expect_equal(r$agg$se, ref$s$overall.se, tolerance = 1e-10)        # same seed, same bootstrap draws
  expect_equal(r$event_study$att, ref$e$att.egt, tolerance = 1e-10)
  expect_equal(r$event_study$se, ref$e$se.egt, tolerance = 1e-10)
}

test_that("pairwise equals did's panel estimator on a balanced panel (dr, no controls)", {
  skip_if_not_installed("did")
  d <- make_gappy(noise = 2, balanced = TRUE)
  r <- cs_fit(d, cs_unbalanced = "pairwise", est_method = "dr", event_study = TRUE)
  expect_did_panel_equal(r, did_panel_ref(d, ~1, "dr"))
})

test_that("pairwise equals did's panel estimator on a balanced panel (reg, with a control)", {
  skip_if_not_installed("did")
  d <- make_gappy(noise = 2, balanced = TRUE)
  d[, y := y + 0.7 * x]
  r <- cs_fit(d, cs_unbalanced = "pairwise", est_method = "reg", controls = "x", event_study = TRUE)
  expect_did_panel_equal(r, did_panel_ref(d, ~x, "reg"))
})

test_that("pairwise is exactly 0 on an additive outcome with level-dependent gaps; rc is not", {
  skip_if_not_installed("did")
  d <- make_gappy()
  expect_lt(nrow(d), data.table::uniqueN(d$id) * data.table::uniqueN(d$year))   # really unbalanced
  pw <- cs_fit(d, cs_unbalanced = "pairwise", est_method = "dr")
  expect_identical(pw$metadata$cs_unbalanced, "pairwise")
  expect_lt(max(abs(pw$raw$att), na.rm = TRUE), 1e-10)
  expect_lt(abs(pw$agg$att), 1e-10)
  rc <- cs_fit(d, cs_unbalanced = "rc", est_method = "dr")
  expect_gt(abs(rc$agg$att), 1e-3)                        # the composition artifact this option removes
})

test_that("pairwise is exactly 0 with a control (reg) on the gapped additive outcome", {
  skip_if_not_installed("did")
  d <- make_gappy()
  pw <- cs_fit(d, cs_unbalanced = "pairwise", est_method = "reg", controls = "x")
  expect_lt(max(abs(pw$raw$att), na.rm = TRUE), 1e-10)
  expect_lt(abs(pw$agg$att), 1e-10)
})

test_that("pairwise event study is exactly 0 at every event time on the gapped additive outcome", {
  skip_if_not_installed("did")
  d <- make_gappy()
  pw <- cs_fit(d, cs_unbalanced = "pairwise", est_method = "dr", event_study = TRUE)
  expect_true(nrow(pw$event_study) > 10)
  expect_lt(max(abs(pw$event_study$att)), 1e-10)
})

test_that("each pairwise ATT(g, t) is the change for units observed in both years, treated minus comparison", {
  skip_if_not_installed("did")
  d <- make_gappy(noise = 3)
  pw <- cs_fit(d, cs_unbalanced = "pairwise", est_method = "dr")
  manual <- function(gv, tv) {
    base <- gv - 1L
    w <- data.table::dcast(d[year %in% c(base, tv)], id + g0 ~ year, value.var = "y")
    data.table::setnames(w, c(as.character(base), as.character(tv)), c("y0", "y1"))
    w <- w[!is.na(y0) & !is.na(y1)]
    tr <- w[g0 == gv]; co <- w[g0 == 0L | (g0 > max(tv, base) & g0 != gv)]
    mean(tr$y1 - tr$y0) - mean(co$y1 - co$y0)
  }
  for (k in list(c(2005L, 2004L + 2L), c(2008L, 2009L), c(2008L, 2003L), c(2011L, 2013L))) {
    i <- which(pw$raw$group == k[1] & pw$raw$t == k[2])
    expect_equal(pw$raw$att[i], manual(k[1], k[2]), tolerance = 1e-10)
  }
})

test_that("pairwise requires allow_unbalanced_panel = TRUE and time-invariant weights", {
  skip_if_not_installed("did")
  d <- make_gappy()
  expect_error(staggeredpower:::.cs_att_gt(
    data = d, yname = "y", tname = "year", idname = "id", gname = "g0",
    allow_unbalanced_panel = FALSE, cs_unbalanced = "pairwise"), "allow_unbalanced_panel")
  d[, w := 1 + (year %% 2)]
  expect_error(staggeredpower:::.cs_att_gt(
    data = d, yname = "y", tname = "year", idname = "id", gname = "g0", weightsname = "w",
    allow_unbalanced_panel = TRUE, cs_unbalanced = "pairwise"), "time-invariant")
})

test_that("CS power DGP built with pairwise recovers the null exactly under pairwise", {
  skip_if_not_installed("did")
  d <- make_gappy(noise = 2)
  res <- suppressWarnings(run_power_analysis(
    data_clean = d, unit_var = "id", group_var = "g", time_var = "year",
    rel_pass_var = "rel_pass", treat_ind_var = "treat_ind",
    controls = NULL, outcome = "y", pta_type = "cs", percent_effect = 1.0,
    models_to_run = "cs", n_sims = 1L, noise_spec = list(engine = "none"),
    allow_unbalanced_panel = TRUE, est_method = "dr", base_period = "universal",
    cs_unbalanced = "pairwise"))
  expect_lt(abs(res$final_power[model == "cs"]$att), 1e-8)
})

test_that("the cs_cohort surface built with pairwise reads exactly 0 under pairwise", {
  skip_if_not_installed("did")
  skip_if_not_installed("fixest")
  d <- make_gappy(noise = 2)
  d[, y := y + 0.3 * (!is.na(g) & g == 2011L) * (year - 2000)]   # cohort-specific trend: non-additive surface
  surf <- function(cu) suppressWarnings(enforce_PTA(
    df = d, unit = "id", group = "g", time = "year", outcome = "y", method = "CS",
    noise_spec = list(engine = "none", obs_model = "poisson", control_mean = "cs_cohort"),
    est_method = "dr", base_period = "universal", allow_unbalanced_panel = TRUE,
    cs_unbalanced = cu))
  s_pw <- surf("pairwise")
  r <- cs_fit(s_pw, y = "cf_mean_rate", cs_unbalanced = "pairwise", est_method = "dr")
  expect_lt(max(abs(r$raw$att[r$raw$t >= r$raw$group]), na.rm = TRUE), 1e-10)
  expect_lt(abs(r$agg$att), 1e-10)
})

test_that("wcr_test(estimator = 'cs') uses the pairwise estimator when asked", {
  skip_if_not_installed("did")
  skip_if_not_installed("fixest")
  d <- make_gappy(noise = 2)
  d[treat_ind == 1L, y := y + 1]
  pw <- cs_fit(d, cs_unbalanced = "pairwise", est_method = "dr")
  w <- wcr_test(d, outcome = "y", unit_var = "id", time_var = "year", group_var = "g0",
                treat_ind_var = "treat_ind", estimator = "cs", B = 9L, seed = 1,
                est_method = "dr", base_period = "universal", allow_unbalanced_panel = TRUE,
                cs_unbalanced = "pairwise")
  expect_equal(w$estimate, pw$agg$att, tolerance = 1e-10)
})
