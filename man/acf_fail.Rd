% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/acf_fail.r
\name{acf_fail}
\alias{acf_fail}
\title{ACF Test failure message}
\usage{
acf_fail(
  udg_list = NULL,
  acf_lags_fail = c(1, 2, 3, 4, 12, 24),
  num_sig = 8,
  include_pacf = TRUE
)
}
\arguments{
\item{udg_list}{List object generated by \code{udg()} function of the \code{seasonal} package.
This is a required entry.}

\item{acf_lags_fail}{Lags of the ACF to test
Default is \code{c(1, 2, 3, 4, 8)}.}

\item{num_sig}{Limit for number of lags with significant ACF values
Default is \code{4}.}

\item{include_pacf}{Logical scalar that indicates if the PACF is included in the testing.
Default is TRUE}
}
\value{
Logical object which is \code{TRUE} if series fails the ACF test, \code{FALSE} otherwise
}
\description{
Tests whether the sample autocorrelation of the residuals from a time series model fails
the Ljung-Box or Box-Pierce Q test.
}
\details{
Version 3.2, 2024-05-15
}
\examples{
ukgas_seas <- seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)",
                             x11="", transform.function = "log", forecast.maxlead=20,
                             check.print = c( "pacf", "pacfplot" ))
ukgas_udg <- seasonal::udg(ukgas_seas)
ukgas_acf_fail <- acf_fail(ukgas_udg, acf_lags_fail = c(1, 2, 3, 4, 8), num_sig = 4)
}
\author{
Brian C. Monsell, \email{monsell.brian@bls.gov} or \email{bcmonsell@gmail.com}
}
