#' NFS Law Adoption Panel Dataset
#'
#' A balanced panel dataset of U.S. states with staggered adoption of
#' non-fatal strangulation (NFS) assault laws. Suitable for demonstrating
#' power analysis in staggered difference-in-differences designs.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{state}{State name}
#'   \item{year}{Calendar year}
#'   \item{treatment_year}{Year the state adopted NFS law (NA if never-treated)}
#'   \item{treated}{Treatment indicator: 1 if post-treatment, 0 otherwise}
#'   \item{rel_time}{Relative time to treatment (year - treatment_year)}
#'   \item{assault_rate}{Aggravated assault rate}
#'   \item{clearance_rate}{Arrest clearance rate for aggravated assaults}
#' }
#'
#' @source Derived from NIBRS (National Incident-Based Reporting System) data
#'   on aggravated assault clearance rates across U.S. states.
"nfs_panel"
