% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_series.r
\name{save_series}
\alias{save_series}
\title{Save Series}
\usage{
save_series(this_series = NULL, this_file = NULL)
}
\arguments{
\item{this_series}{double precision time series array to be saved.
This is a required entry.}

\item{this_file}{character string; name of file time series array to be saved to.
This is a required entry.}
}
\value{
file with user-defined regressors will be produced
}
\description{
Save a user-defined regression array or matrix with time series attributes to an external ASCII file
in X-13ARIMA-SEATS' datevalue format
}
\details{
Version 1.2, 5/25/2023
}
\examples{
ukgas_seas <- 
   seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
                  x11="", transform.function = "log", forecast.maxlead=20, 
                  slidingspans = "", check.print = c( "pacf", "pacfplot" ))
ukgas_sa <- seasonal::final(ukgas_seas)
\dontrun{save_series(ukgas_sa, 'ukgas_sa.txt')}
}
\author{
Brian C. Monsell, \email{monsell.brian@bls.gov} or \email{bcmonsell@gmail.com}
}
