% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_seasonal_ftest_all.r
\name{get_seasonal_ftest_all}
\alias{get_seasonal_ftest_all}
\title{Generate model based F-test}
\usage{
get_seasonal_ftest_all(
  seas_obj = NULL,
  this_series = "b1",
  use_seasonal = TRUE
)
}
\arguments{
\item{seas_obj}{\code{seas} object for a single series.
This is a required entry.}

\item{this_series}{character string; the table used to generate the model based F-test.
Default is \code{"b1"}.}

\item{use_seasonal}{logical scalar; if TRUE, the \code{seasonal} regressor is used;
otherwise, use the sine-cosine trignonmetric regressors generated by \code{sincos}.}
}
\value{
a numeric vector with the degrees of freedom, F statistic, and probability
generated for the model based seasonal f-test used in the seasonal testing
procedure in Maravall(2012)
}
\description{
Generate model based F-test, changing the model to remove seasonal differences and adding
seasonal regressors if necessary. This function is used in the overall seasonal test
from Maravall (2012)
}
\details{
Version 4.3, 6/21/2024
}
\examples{
air_seas <- 
     seasonal::seas(AirPassengers, arima.model="(0 1 1)(0 1 1)", 
                    forecast.maxlead = 36, slidingspans = "", 
                    transform.function = "log", series.save = "a1")
air_ftest_all <- 
    get_seasonal_ftest_all(air_seas, this_series = "a1")
}
\author{
Brian C. Monsell, \email{monsell.brian@bls.gov} or \email{bcmonsell@gmail.com}
}
