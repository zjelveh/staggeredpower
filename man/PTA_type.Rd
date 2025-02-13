% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enforce_pta.R
\name{PTA_type}
\alias{PTA_type}
\title{Enforce parallel trends assumption (PTA)}
\usage{
PTA_type(
  df_wide,
  period,
  cohort,
  unit,
  group,
  time,
  g,
  rp,
  enforce_type,
  rho_hat,
  sd_resid,
  time_fe,
  unit_fe,
  max_year,
  min_year
)
}
\arguments{
\item{df_wide}{The wide format data frame}

\item{unit}{The column name for the unit variable}

\item{group}{The column name for the group variable}

\item{time}{The column name for the time variable}

\item{g}{The group being treated}

\item{rp}{The relative period (time since treatment)}

\item{min_year}{The earliest year in the data}

\item{pta_type}{The type of parallel trends assumption to enforce. Options are 'pre_year', 'pre_years', 'max_untreated_year', and 'diffs'.}
}
\value{
The modified wide format data frame with the parallel trends assumption enforced
}
\description{
This function enforces different types of parallel trends assumptions on a wide format data frame.
}
\examples{
df_wide <- PTA_type(df_wide, "pre_year", "unit", "group", "time", 1, 0, 2000)
}
