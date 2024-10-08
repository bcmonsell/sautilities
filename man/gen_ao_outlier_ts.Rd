% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_ao_outlier_ts.r
\name{gen_ao_outlier_ts}
\alias{gen_ao_outlier_ts}
\title{Generate level change regression variable as a \code{ts} object}
\usage{
gen_ao_outlier_ts(
  ao_date,
  this_start,
  this_end,
  this_freq = 12,
  return_matrix = TRUE
)
}
\arguments{
\item{ao_date}{Integer vector of length two - dates for AO outlier to be generated.}

\item{this_start}{Numeric vector; start date of AO outlier regressor generated.}

\item{this_end}{Numeric vector; end date of AO outlier regressor generated.}

\item{this_freq}{Numeric scalar; frequency of time series.
Default: \code{12}, for a monthly series.}

\item{return_matrix}{Logical scalar; If true, the object returned is a one column time series matrix object.
Default: \code{TRUE}}
}
\value{
A \code{ts} object of a point outlier regressor.
}
\description{
Generates a \code{ts} object for a AO (point) outlier regressor.
}
\details{
Version 1.7, 5/23/2023
}
\examples{
UKgas_ao_date <- c(1970, 2)
UKgas_ao_1970_2 <-
   gen_ao_outlier_ts(UKgas_ao_date, this_start = c(1960, 1), this_end = c(1990, 4), 
                     this_freq = 4)
}
\author{
Brian C. Monsell, \email{monsell.brian@bls.gov} or \email{bcmonsell@gmail.com}
}
