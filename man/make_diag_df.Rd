% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_diag_df.r
\name{make_diag_df}
\alias{make_diag_df}
\title{Generate diagnostic summary data frame}
\usage{
make_diag_df(
  this_data_names = NULL,
  this_acf_test = NULL,
  this_d11f_test = NULL,
  this_spec_peak_test = NULL,
  this_spec_peak_ori_test = NULL,
  this_qs_test = NULL,
  this_qs_rsd_test = NULL,
  this_qs_seasonal_test = NULL,
  this_model_test = NULL,
  this_sspan_test = NULL,
  this_m7_test = NULL,
  this_q2_test = NULL,
  return_this = "both"
)
}
\arguments{
\item{this_data_names}{vector object with names of time series used in seasonal adjustment.
This is a required entry.}

\item{this_acf_test}{list object with results from test of regARIMA residual ACF}

\item{this_d11f_test}{list object with results from test of D11F}

\item{this_spec_peak_test}{list object with results from testing for spectral peaks
in the seasonally adjusted series}

\item{this_spec_peak_ori_test}{list object with results from testing for spectral peaks
in the original series}

\item{this_qs_test}{list object with results from QS test}

\item{this_qs_rsd_test}{list object with results from residual QS test}

\item{this_qs_seasonal_test}{list object with results from seasonal QS test}

\item{this_model_test}{list object with results from model diagnostics test}

\item{this_sspan_test}{list object with results from sliding spans test}

\item{this_m7_test}{list object with results from M7 test}

\item{this_q2_test}{list object with results from Q2 test}

\item{return_this}{Character string; what the function returns -
\code{'why'} returns why the test failed or received a warning,
\code{'test'} returns test results, or \code{'both'}.
Default is \code{'both'}.}
}
\value{
A data frame with X-13 Diagnostics, with the elements not expressed as factors
}
\description{
Generates a data frame with seasonal adjustment and modeling diagnostics.
}
\details{
Version 6.0, 5/21/2024
}
\examples{
unemp_seasonal_filter <- lapply(unemployment_list, function(x) 
     try(optimal_seasonal_filter(x, use_msr = TRUE)))
unemp_seas_list <- seasonal::seas(unemployment_list,
                     x11 = "", slidingspans = "",
                     arima.model = "(0 1 1)(0 1 1)",
                     transform.function = "log",
                     forecast.maxlead=60,
                     check.print = c( "pacf", "pacfplot" ),
                     list = list(
		          list(x11.seasonalma = unemp_seasonal_filter$n3000013),
		          list(x11.seasonalma = unemp_seasonal_filter$n3000014),
		          list(x11.seasonalma = unemp_seasonal_filter$n3000025),
		          list(x11.seasonalma = unemp_seasonal_filter$n3000026)
                     ))
unemp_seas_update <- 
     Filter(function(x) inherits(x, "seas"), unemp_seas_list)
unemp_acf <- lapply(unemp_seas_update, function(x) 
  try(acf_test(x, return_this = 'both')))
unemp_d11f <- lapply(unemp_seas_update, function(x) 
  try(d11f_test(x, p_level = 0.05, return_this = 'both')))
unemp_spec_peak <- lapply(unemp_seas_update, function(x) 
  try(spec_peak_test(x, return_this = 'both')))
unemp_spec_peak_ori <- lapply(unemp_seas_update, function(x) 
  try(spec_peak_test(x, this_spec = "spcori", return_this = 'both')))
unemp_qs <- lapply(unemp_seas_update, function(x) 
  try(qs_test(x, test_full = FALSE, p_limit_fail = 0.01,
              p_limit_warn = 0.05, return_this = 'both')))
unemp_qs_rsd <- lapply(unemp_seas_update, function(x) 
  try(qs_rsd_test(x, test_full = FALSE, p_limit_fail = 0.01,
                  p_limit_warn = 0.05, return_this = 'both')))
unemp_qs_seasonal <- lapply(unemp_seas_update, function(x) 
  try(qs_seasonal_test(x, test_full = FALSE,
                       p_limit_pass = 0.01, p_limit_warn = 0.05, 
                       robust_sa = FALSE, return_this = 'both')))
unemp_model <- lapply(unemp_seas_update, function(x) 
  try(model_test(x, return_this = 'both')))
unemp_sspan <- lapply(unemp_seas_update, function(x) 
  try(sspan_test(x, sf_limit = 15, change_limit = 35, 
      return_this = 'both')))
unemp_m7 <- lapply(unemp_seas_update, function(x) 
  try(mq_test(x, return_this = 'both')))
unemp_q2 <- lapply(unemp_seas_update, function(x) 
  try(mq_test(x, this_label = 'Q2', return_this = 'both')))
unemp_names <- names(unemployment_list)
unemp_diag_df <-
    make_diag_df(unemp_names, 
                 this_acf_test = unemp_acf, 
                 this_d11f_test = unemp_d11f, 
                 this_spec_peak_test = unemp_spec_peak, 
                 this_spec_peak_ori_test = unemp_spec_peak_ori,
                 this_qs_test = unemp_qs, 
                 this_qs_rsd_test = unemp_qs_rsd,
                 this_qs_seasonal_test = unemp_qs_seasonal, 
                 this_model_test = unemp_model,
                 this_sspan_test = unemp_sspan, 
                 this_m7_test = unemp_m7, 
                 this_q2_test = unemp_q2)

}
\author{
Brian C. Monsell, \email{monsell.brian@bls.gov} or \email{bcmonsell@gmail.com}
}
