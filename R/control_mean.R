# R/control_mean.R
# ---------------------------------------------------------------------------
# Modular control-mean surfaces for the re-roll-controls power DGP.
#
# When noise_spec$reroll_controls is TRUE, the power simulation regenerates
# outcomes for BOTH treated and control units (see the obs block in
# run_power_analysis()). Treated units keep the harmonized .cs_delta_control
# counterfactual (cf_mean_rate); control units need their own mean surface,
# supplied by one of these pluggable functions.
#
# INTERFACE (every surface shares this exact signature):
#   fn(df, unit, time, outcome, treat_ind, pop_var = NULL) -> numeric vector,
#      one fitted MEAN (outcome / rate scale) per row of df. Only the CONTROL
#      rows' values are used downstream; treated rows keep cf_mean_rate.
#      Surfaces must be parallel-trends-consistent.
#
# TO ADD A SURFACE (next iterations): write cm_<name>() with the signature
# above and add it to .CONTROL_MEAN_SURFACES. Nothing else changes. Select at
# run time via noise_spec$control_mean = "<name>". Planned surfaces:
#   cm_did      - derived from did's own control-side (tighter CS match)
#   cm_block    - state-block residual resampling (diagnostic-3's DGP "C")
#   cm_meandep  - state-specific / mean-dependent dispersion
# ---------------------------------------------------------------------------

#' Two-way (unit + year) fitted surface from untreated cells
#'
#' \eqn{S(i,t)=\widehat\alpha_i+\widehat\gamma_t}, fit on untreated observations
#' with \code{fixest::feols} -- the SAME routine \code{calibrate_noise} and the
#' imputation estimator use, so the reroll surface and the estimator's own
#' counterfactual are the identical fit (they agree exactly wherever the design
#' is full rank; they differ only at unbalanced-panel edge cells). \code{feols}
#' returns NA for a cell whose unit or year level is absent from the untreated
#' fit; those fall back to the observed outcome (for control cells the observed
#' value IS the no-law mean).
#' @keywords internal
.twfe_fitted_untreated <- function(d, unit, time, outcome, untreated) {
  fml <- stats::as.formula(sprintf("`%s` ~ 1 | `%s` + `%s`", outcome, unit, time))
  fit <- fixest::feols(fml, data = d[untreated], notes = FALSE)
  S <- as.numeric(stats::predict(fit, newdata = d))
  bad <- !is.finite(S)
  if (any(bad)) S[bad] <- as.numeric(d[[outcome]])[bad]
  S
}

#' TWFE control-mean surface (default)
#'
#' Additive unit + year fixed effects fit on untreated observations only,
#' predicted for every row. Consistent with the additive parallel-trends both
#' the CS and imputation estimators assume.
#' @keywords internal
cm_twfe <- function(df, unit, time, outcome, treat_ind, pop_var = NULL) {
  d <- data.table::as.data.table(df)
  untreated <- d[[treat_ind]] != 1
  .twfe_fitted_untreated(d, unit, time, outcome, untreated)
}

#' CS-anchored control-mean surface (for the CS DGP)
#'
#' Anchors each eventually-treated unit's untreated mean to its REALIZED
#' pre-adoption value plus the common time trend,
#' \eqn{\mu(i,t) = Y_i(g-1) + (\gamma_t - \gamma_{g-1})}, matching the anchor
#' \code{enforce_PTA_CS} builds the treated counterfactual on. \code{cm_twfe}
#' instead centers the pre-period on the unit's AVERAGE untreated level
#' (\eqn{\alpha_i + \gamma_{g-1}}); the gap is the anchor residual
#' \eqn{r_i = Y_i(g-1) - (\alpha_i + \gamma_{g-1})}, a constant per-unit shift
#' that cancels in every change (so control roles / parallel trends are
#' preserved) but fixes the treated unit's anchor so CS is unbiased. The residual
#' is what distinguishes CS from imputation, so this keeps the CS DGP distinct.
#' Never-treated units have no anchor and fall back to \eqn{\alpha_i + \gamma_t}.
#' @keywords internal
cm_cs_anchor <- function(df, unit, time, outcome, treat_ind, pop_var = NULL) {
  d <- data.table::as.data.table(df)
  u <- as.character(d[[unit]]); tm <- d[[time]]; y <- d[[outcome]]; tr <- d[[treat_ind]]
  untreated <- tr != 1
  S <- .twfe_fitted_untreated(d, unit, time, outcome, untreated)   # S(i,t) = alpha_i + gamma_t (feols)
  # cohort g_i = first treated year per unit (eventually-treated); NA for never-treated
  g_by_unit <- tapply(tm[tr == 1], u[tr == 1], min)
  g <- as.numeric(g_by_unit[u])                                    # per-row cohort (NA = never-treated)
  # anchor Y_i(g-1) and S_i(g-1) from the pre-adoption row of each treated unit
  is_anchor <- !is.na(g) & tm == (g - 1)
  anchor_y <- tapply(y[is_anchor], u[is_anchor], function(z) z[1])
  anchor_S <- tapply(S[is_anchor], u[is_anchor], function(z) z[1])
  ay <- as.numeric(anchor_y[u]); aS <- as.numeric(anchor_S[u])
  # mu(i,t) = Y_i(g-1) + (gamma_t - gamma_{g-1}) = ay + (S - aS); never-treated / no anchor -> S
  as.numeric(ifelse(!is.na(g) & !is.na(ay), ay + (S - aS), S))
}

# Registry of available control-mean surfaces. ADD NEW SURFACES HERE.
.CONTROL_MEAN_SURFACES <- list(
  twfe      = cm_twfe,
  cs_anchor = cm_cs_anchor
)

#' Look up a control-mean surface by name (default "twfe").
#' @keywords internal
get_control_mean_surface <- function(name = "twfe") {
  if (is.null(name)) name <- "twfe"
  fn <- .CONTROL_MEAN_SURFACES[[name]]
  if (is.null(fn)) {
    stop(sprintf("Unknown control_mean surface '%s'. Available: %s",
                 name, paste(names(.CONTROL_MEAN_SURFACES), collapse = ", ")))
  }
  fn
}

#' Rough default latent-shock log-SD (fallback only)
#'
#' Callers should pass a calibrated \code{noise_spec$latent_sigma} (log-scale SD
#' of the multiplicative latent shock) estimated from the untreated overdispersion
#' with the mean structure removed. This fallback is deliberately crude and errs
#' toward zero; it exists so the DGP does not crash when latent_sigma is omitted.
#' @keywords internal
latent_sigma_default <- function(df, outcome, pop_var, treat_ind) {
  0.0
}
