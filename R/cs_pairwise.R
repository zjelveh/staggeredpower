# R/cs_pairwise.R
# Callaway-Sant'Anna on an unbalanced panel, one comparison at a time.
#
# With allow_unbalanced_panel = TRUE on a panel with gaps, did (2.3.0) sets panel = FALSE
# and estimates every ATT(g, t) with the repeated-cross-section 2x2 estimators
# (DRDID::drdid_rc and siblings): the base-year mean and the year-t mean are taken over
# different sets of units, so unit effects do not cancel. cs_unbalanced = "pairwise"
# instead estimates each ATT(g, t) with did's panel 2x2 estimator on the units observed
# in both of its years. Every unit is kept wherever it has the two years a comparison
# needs; on a balanced panel the result is did's panel estimator exactly.
#
# Mechanism: the unit x period grid is completed with rows whose outcome is Inf ("not
# observed"; did keeps them, since complete.cases() treats Inf as present), so did runs
# its own panel code (aggregation, bootstrap and event study unchanged). The 2x2 step is
# a did custom est_method that drops the units whose outcome is Inf in either year of
# the comparison and pads their influence function with zeros, scaled as did scales it.

#' did custom est_method: the panel 2x2 on the units observed in both years
#' @noRd
.cs_pairwise_2x2 <- function(est_method) {
  est <- switch(est_method,
                dr  = DRDID::drdid_panel,
                reg = DRDID::reg_did_panel,
                ipw = DRDID::std_ipw_did_panel)
  function(y1, y0, D, covariates, i.weights, inffunc = TRUE) {
    n1 <- length(y1)
    ok <- is.finite(y1) & is.finite(y0)
    inf <- numeric(n1)
    # no treated or no comparison unit observed in both years: not estimable (NA, which
    # aggte(na.rm = TRUE) drops; did would turn NaN into an ATT of 0)
    if (!any(D[ok] == 1) || !any(D[ok] == 0)) return(list(ATT = NA_real_, att.inf.func = inf))
    r <- est(y1 = y1[ok], y0 = y0[ok], D = D[ok],
             covariates = as.matrix(covariates)[ok, , drop = FALSE],
             i.weights = i.weights[ok], boot = FALSE, inffunc = TRUE)
    # did multiplies by n / n1 afterwards, so a unit's weight in the aggregate influence
    # function is n / (units observed in both years), as for did's own units
    inf[ok] <- (n1 / sum(ok)) * r$att.inf.func
    list(ATT = r$ATT, att.inf.func = inf)
  }
}

#' Complete the unit x period grid; added rows carry outcome Inf
#'
#' Rows missing a value did needs are dropped first (as did would). Added rows copy every
#' other column from the unit's first row: gname, weights and cluster are time-invariant,
#' and the covariates of an added row never enter a comparison (did takes covariates from
#' one of the comparison's two years, both observed for every unit it keeps).
#' @noRd
.cs_complete_grid <- function(data, yname, tname, idname, gname, xvars, weightsname, clustervars) {
  d <- data.table::as.data.table(data)
  need <- unique(c(idname, tname, gname, yname, xvars, weightsname, clustervars))
  keep <- stats::complete.cases(d[, need, with = FALSE])
  if (!all(keep)) warning("dropped ", sum(!keep), " rows from original data due to missing data", call. = FALSE)
  d <- d[keep]
  if (any(!is.finite(d[[yname]]))) stop("cs_unbalanced = 'pairwise': the outcome must be finite.", call. = FALSE)
  if (anyDuplicated(d[, c(idname, tname), with = FALSE])) {
    stop("cs_unbalanced = 'pairwise': a unit is observed more than once in a period.", call. = FALSE)
  }
  for (v in c(weightsname, setdiff(clustervars, idname))) {
    if (any(d[, data.table::uniqueN(get(v)), by = c(idname)]$V1 > 1L)) {
      stop("cs_unbalanced = 'pairwise': ", v, " must be time-invariant within unit ",
           "(did's panel estimator takes one value per unit).", call. = FALSE)
    }
  }
  data.table::setorderv(d, c(idname, tname))
  grid <- data.table::CJ(.id = unique(d[[idname]]), .t = sort(unique(d[[tname]])))
  data.table::setnames(grid, c(".id", ".t"), c(idname, tname))
  miss <- grid[!d, on = c(idname, tname)]
  if (nrow(miss) == 0L) return(d)
  first <- d[, .SD[1L], by = c(idname)]
  add <- first[, setdiff(names(first), tname), with = FALSE][miss, on = idname]
  add[, (yname) := Inf]
  out <- data.table::rbindlist(list(d, add), use.names = TRUE)
  data.table::setorderv(out, c(idname, tname))
  out[]
}

#' did::att_gt with a choice of estimator for unbalanced panels
#'
#' @param cs_unbalanced "rc" (default): did as is (on an unbalanced panel with
#'   allow_unbalanced_panel = TRUE, did's repeated-cross-section estimator). "pairwise":
#'   each ATT(g, t) on the units observed in both of its years, with did's panel 2x2
#'   estimator; requires allow_unbalanced_panel = TRUE and est_method "dr", "reg" or "ipw".
#' @param ... Further did::att_gt arguments.
#' @noRd
.cs_att_gt <- function(data, yname, tname, idname, gname, xformla = ~1, weightsname = NULL,
                       clustervars = NULL, allow_unbalanced_panel = FALSE,
                       cs_unbalanced = c("rc", "pairwise"), est_method = "dr", ...) {
  cs_unbalanced <- match.arg(cs_unbalanced)
  if (is.null(xformla)) xformla <- ~1
  if (cs_unbalanced == "rc") {
    return(did::att_gt(yname = yname, tname = tname, idname = idname, gname = gname,
                       xformla = xformla, data = as.data.frame(data), weightsname = weightsname,
                       clustervars = clustervars, allow_unbalanced_panel = allow_unbalanced_panel,
                       est_method = est_method, ...))
  }
  if (!isTRUE(allow_unbalanced_panel)) {
    stop("cs_unbalanced = 'pairwise' keeps units with gaps, so it requires ",
         "allow_unbalanced_panel = TRUE.", call. = FALSE)
  }
  if (!(is.character(est_method) && length(est_method) == 1L && est_method %in% c("dr", "reg", "ipw"))) {
    stop("cs_unbalanced = 'pairwise' needs est_method 'dr', 'reg' or 'ipw'.", call. = FALSE)
  }
  d <- .cs_complete_grid(data, yname, tname, idname, gname, all.vars(xformla),
                         weightsname, clustervars)
  # did sets never-treated (gname 0) to Inf inside a copy; an integer column cannot hold Inf
  d[, (gname) := as.numeric(get(gname))]
  did::att_gt(yname = yname, tname = tname, idname = idname, gname = gname,
              xformla = xformla, data = as.data.frame(d), panel = TRUE,
              allow_unbalanced_panel = FALSE, weightsname = weightsname,
              clustervars = clustervars, est_method = .cs_pairwise_2x2(est_method), ...)
}
