% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_udg_entry.r
\name{get_udg_entry}
\alias{get_udg_entry}
\title{Get entry from UDG file}
\usage{
get_udg_entry(seas_obj = NULL, this_key, this_index = 0, convert = TRUE)
}
\arguments{
\item{seas_obj}{\code{seas} object for a single series}

\item{this_key}{character scalar; keyword found in UDG output generated by \code{X-13ARIMA-SEATS}.}

\item{this_index}{integer scalar; index of entry in vector to extract. If set to 0 (the default),
get the last entry.}

\item{convert}{logical scalar; if \code{TRUE}, convert character to numeric object.
Default is \code{TRUE}.}
}
\value{
The \code{this_index} element of the array returned from the UDG entry for \code{this_key}
}
\description{
Returns a specific element of a list of \code{udg} entries.
}
\details{
Version 2.7 5/14/2024
}
\examples{
air_seas_short <- seasonal::seas(AirPassengers, series.span = ',1960.9', 
                    arima.model="(0 1 1)(0 1 1)", 
                    forecast.maxlead = 36, slidingspans = "", 
                    transform.function = "log")
fcst_tstat <- get_udg_entry(air_seas_short, 'forctval01')
}
\author{
Brian C. Monsell, \email{monsell.brian@bls.gov} or \email{bcmonsell@gmail.com}
}
