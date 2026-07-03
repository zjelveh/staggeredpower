# R/event_study_sample.R
#' Build a balanced-composition event-study sample
#'
#' Restricts a staggered-adoption panel to a set of units that support a
#' non-compositional event study over a displayed window. A treated unit is
#' retained only if it is observed at EVERY event time in
#' \code{c((-L):-1, 0:K)}, where event time is \code{time - group}. All
#' never-treated units are retained. This is a UNIT-level (not cohort-level)
#' filter: within-cohort gaps otherwise let a partially-observed unit contribute
#' to only some event-time cells, which is exactly the composition problem an
#' event study must avoid.
#'
#' Use the returned \code{df_es} as the estimation sample for BOTH the CS and the
#' imputation/did2s event studies, so the two figures are compared on identical
#' composition. (For CS, \code{did}'s \code{balance_e} only balances the
#' post-treatment side; restricting the sample balances pre and post and works
#' for estimators that have no \code{balance_e} argument.)
#'
#' @param data data.table or data.frame. Panel data.
#' @param id_var Character. Unit identifier variable name.
#' @param time_var Character. Time variable name.
#' @param group_var Character. Treatment cohort variable name (adoption time;
#'   never-treated coded as \code{never_treated_value}).
#' @param L Integer. Largest pre-treatment lead displayed (require leads
#'   \code{-L, ..., -1}).
#' @param K Integer. Largest post-treatment lag displayed (require lags
#'   \code{0, ..., K}).
#' @param never_treated_value Value of \code{group_var} for never-treated units
#'   (default 0; \code{NA} also treated as never-treated).
#'
#' @return A list with:
#'   \describe{
#'     \item{df_es}{The restricted sample (never-treated + eligible treated
#'       units), as a data.table.}
#'     \item{support}{Per-treated-unit audit table: id, group, first/last time,
#'       observed event times, missing required event times, and
#'       \code{has_all_required}.}
#'   }
#'
#' @examples
#' \donttest{
#' # es <- build_event_study_sample(nfs_panel, "state", "year",
#' #                                "treatment_year", L = 3, K = 3)
#' # es$support   # which treated units are kept/dropped and why
#' # es$df_es     # feed to estimate_models(event_study = TRUE, ...)
#' }
#' @export
build_event_study_sample <- function(data, id_var, time_var, group_var,
                                     L, K, never_treated_value = 0) {
  stopifnot(is.character(id_var), is.character(time_var), is.character(group_var),
            length(L) == 1L, length(K) == 1L, L >= 1L, K >= 0L)
  d <- data.table::as.data.table(data)

  # Normalize never-treated (NA -> never_treated_value) for the treated test
  is_treated <- !is.na(d[[group_var]]) & d[[group_var]] != never_treated_value

  required_es <- c(seq.int(-L, -1L), seq.int(0L, K))

  treated <- d[is_treated]
  treated[, .event_time := get(time_var) - get(group_var)]

  support <- treated[, {
    ets <- sort(unique(.event_time))
    miss <- setdiff(required_es, ets)
    list(
      group            = get(group_var)[1],
      first_time       = min(get(time_var)),
      last_time        = max(get(time_var)),
      event_times      = paste(ets, collapse = ", "),
      missing_required = paste(miss, collapse = ", "),
      has_all_required = length(miss) == 0L
    )
  }, by = c(id_var)]

  eligible_ids <- support[has_all_required == TRUE][[id_var]]

  keep <- (!is_treated) | (d[[id_var]] %in% eligible_ids)
  list(df_es = d[keep], support = support[])
}
