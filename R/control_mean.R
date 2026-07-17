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

#' TWFE control-mean surface (default)
#'
#' Additive unit + year fixed effects fit on untreated observations only,
#' predicted for every row. Consistent with the additive parallel-trends both
#' the CS and imputation estimators assume.
#' @keywords internal
cm_twfe <- function(df, unit, time, outcome, treat_ind, pop_var = NULL) {
  d <- data.table::as.data.table(df)
  untreated <- d[[treat_ind]] != 1
  fml <- stats::as.formula(sprintf("`%s` ~ factor(`%s`) + factor(`%s`)", outcome, unit, time))
  fit <- stats::lm(fml, data = d[untreated])
  as.numeric(stats::predict(fit, newdata = d))
}

# Registry of available control-mean surfaces. ADD NEW SURFACES HERE.
.CONTROL_MEAN_SURFACES <- list(
  twfe = cm_twfe
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
