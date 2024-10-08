% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_window.R
\name{get_window}
\alias{get_window}
\title{Subspan time series}
\usage{
get_window(X = NULL, plot_start = NULL, plot_end = NULL)
}
\arguments{
\item{X}{Time Series object.
This is a required entry.}

\item{plot_start}{Integer vector of length 2; Starting date for plot.
Default is starting date for the time series.}

\item{plot_end}{Integer vector of length 2; Starting date for plot.
Default is ending date for the time series.}
}
\value{
generate subspan of time series \code{X} specified by \code{plot_start} and
\code{plot_end}.
}
\description{
Generate subspan of time series
}
\details{
Version 2.3, 5/25/2023
}
\examples{
air50 <- get_window(AirPassengers, plot_start = c(1950,1), plot_end = c(1959,12))
}
\author{
Brian C. Monsell, \email{monsell.brian@bls.gov} or \email{bcmonsell@gmail.com}
}
