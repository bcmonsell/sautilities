% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_summary_list.r
\name{model_summary_list}
\alias{model_summary_list}
\title{ARIMA model summary from a list}
\usage{
model_summary_list(
  seas_obj_list = NULL,
  add_diff = FALSE,
  return_data_frame = FALSE
)
}
\arguments{
\item{seas_obj_list}{list of \code{seas} objects generated from a call of \code{seas}
on a single time series. A required argument.}

\item{add_diff}{logical scalar; add differencing information, if included in model}

\item{return_data_frame}{logical scalar; if \code{TRUE}, return a data frame of the diagnostics,
otherwise return a matrix. Default is \code{FALSE}.}
}
\value{
matrix of ARMA model orders, nonseasonal and seasonal total parameters
for a given set of series
}
\description{
Generate a matrix that summarizes ARIMA models from a list of \code{seas} objects.
}
\details{
Version 2.1, 5/22/2024
}
\examples{
unemp_seas_list <- 
   seasonal::seas(unemployment_list, slidingspans = "", 
                  transform.function = "log", 
                  outlier.types = "all",
                  arima.model = "(0 1 1)(0 1 1)",
                  forecast.maxlead=36, x11 = "",
                  check.print = c( "pacf", "pacfplot" ))
unemp_seas_update <- 
     Filter(function(x) inherits(x, "seas"), unemp_seas_list)
unemp_model_matrix <- 
    model_summary_list(unemp_seas_update)
unemp_model_df <- 
    model_summary_list(unemp_seas_update, add_diff = TRUE, 
                       return_data_frame = TRUE)
}
\author{
Brian C. Monsell, \email{monsell.brian@bls.gov} or \email{bcmonsell@gmail.com}
}
