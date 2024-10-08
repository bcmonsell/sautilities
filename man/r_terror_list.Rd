% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/r_terror_list.r
\name{r_terror_list}
\alias{r_terror_list}
\title{TERROR for R (applied to a list of series)}
\usage{
r_terror_list(
  this_data_list = NULL,
  this_lead = 36,
  this_log = TRUE,
  this_aictest = NULL,
  this_terror_lags = 1
)
}
\arguments{
\item{this_data_list}{List of time series (all series in list should be the same frequency and
have the same ending date.)}

\item{this_lead}{Number of forecasts generated by the seas run. Default is 36.}

\item{this_log}{logical scalar, if TRUE \code{transform.function} will be set to \code{log}
in the call to the \code{seas} function, otherwise \code{auto} will be used.
Default is \code{TRUE}.}

\item{this_aictest}{a character string with the entries for the \code{regression.aictest}
argument to the \code{seas} function from the \code{seasonal} package.
Default is \code{NULL}.}

\item{this_terror_lags}{Integer scalar for number of forecast lags from the end of series
where t-statistics are collected. Must be either 1, 2, or 3.}
}
\value{
list of t-statistics generated by out of sample forecast error for the last 1 to 3
observation of each series in the list.
}
\description{
Function that duplicates the functionality of the TERROR software (Caporello and Maravall 2004)
that performs quality control on time series based on one step ahead forecasts
}
\details{
Version 3.5, 5/22/2023
}
\examples{
emp_terror <- r_terror_list(employment_list, this_log = FALSE,
                            this_lead = 60, this_terror_lags = 3) 
}
\author{
Brian C. Monsell, \email{monsell.brian@bls.gov} or \email{bcmonsell@gmail.com}
}
