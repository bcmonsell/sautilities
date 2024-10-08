% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/static_with_outlier.r
\name{static_with_outlier}
\alias{static_with_outlier}
\title{Add outliers to \code{seas} object}
\usage{
static_with_outlier(
  seas_obj = NULL,
  new_data = NULL,
  outlier_span = ",",
  outlier_types = "ao,ls"
)
}
\arguments{
\item{seas_obj}{\code{seasonal} object. This is a required entry.}

\item{new_data}{time series object; updated data set from the data used to generate
\code{seas_obj}. This is a required entry.}

\item{outlier_span}{character string; sets the argument \code{outlier.span}.
Default is \code{",")}.}

\item{outlier_types}{character string; sets the argument \code{outlier.types}.
Default is \code{"ao,ls")}.}
}
\value{
An updated static \code{seas} object with \code{outlier} arguments included.
}
\description{
Add arguments from the \code{outlier} spec to a \code{seas} object.
}
\details{
Version 2.9, 5/14/2024
}
\examples{
shoes_seas <- 
   seasonal::seas(shoes2007, slidingspans = "", transform.function = "log", x11 = "",
                   forecast.maxlead = 60)
shoes_seas_outlier <- 
   static_with_outlier(shoes_seas, shoes2008, outlier_types = 'all')
}
\author{
Brian C. Monsell, \email{monsell.brian@bls.gov} or \email{bcmonsell@gmail.com}
}
