#' Generate diagnostic summary data frame
#'
#' Generates a data frame with seasonal adjustment and modeling diagnostics.
#'
#' Version 6.0, 5/21/2024
#'
#' @param this_data_names vector object with names of time series used in seasonal adjustment.
#'        This is a required entry.
#' @param this_acf_test list object with results from test of regARIMA residual ACF
#' @param this_d11f_test list object with results from test of D11F
#' @param this_spec_peak_test list object with results from testing for spectral peaks 
#'        in the seasonally adjusted series
#' @param this_spec_peak_ori_test list object with results from testing for spectral peaks 
#'        in the original series
#' @param this_qs_test list object with results from QS test
#' @param this_qs_rsd_test list object with results from residual QS test
#' @param this_qs_seasonal_test list object with results from seasonal QS test
#' @param this_model_test list object with results from model diagnostics test
#' @param this_sspan_test list object with results from sliding spans test
#' @param this_m7_test list object with results from M7 test
#' @param this_q2_test list object with results from Q2 test
#' @param return_this Character string; what the function returns - 
#'        \code{'why'} returns why the test failed or received a warning,
#'        \code{'test'} returns test results, or \code{'both'}.
#'        Default is \code{'both'}.
#' @return A data frame with X-13 Diagnostics, with the elements not expressed as factors
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' unemp_seasonal_filter <- lapply(unemployment_list, function(x) 
#'      try(optimal_seasonal_filter(x, use_msr = TRUE)))
#' unemp_seas_list <- seasonal::seas(unemployment_list,
#'                      x11 = "", slidingspans = "",
#'                      arima.model = "(0 1 1)(0 1 1)",
#'                      transform.function = "log",
#'                      forecast.maxlead=60,
#'                      check.print = c( "pacf", "pacfplot" ),
#'                      list = list(
#' 		          list(x11.seasonalma = unemp_seasonal_filter$n3000013),
#' 		          list(x11.seasonalma = unemp_seasonal_filter$n3000014),
#' 		          list(x11.seasonalma = unemp_seasonal_filter$n3000025),
#' 		          list(x11.seasonalma = unemp_seasonal_filter$n3000026)
#'                      ))
#' unemp_seas_update <- 
#'      Filter(function(x) inherits(x, "seas"), unemp_seas_list)
#' unemp_acf <- lapply(unemp_seas_update, function(x) 
#'   try(acf_test(x, return_this = 'both')))
#' unemp_d11f <- lapply(unemp_seas_update, function(x) 
#'   try(d11f_test(x, p_level = 0.05, return_this = 'both')))
#' unemp_spec_peak <- lapply(unemp_seas_update, function(x) 
#'   try(spec_peak_test(x, return_this = 'both')))
#' unemp_spec_peak_ori <- lapply(unemp_seas_update, function(x) 
#'   try(spec_peak_test(x, this_spec = "spcori", return_this = 'both')))
#' unemp_qs <- lapply(unemp_seas_update, function(x) 
#'   try(qs_test(x, test_full = FALSE, p_limit_fail = 0.01,
#'               p_limit_warn = 0.05, return_this = 'both')))
#' unemp_qs_rsd <- lapply(unemp_seas_update, function(x) 
#'   try(qs_rsd_test(x, test_full = FALSE, p_limit_fail = 0.01,
#'                   p_limit_warn = 0.05, return_this = 'both')))
#' unemp_qs_seasonal <- lapply(unemp_seas_update, function(x) 
#'   try(qs_seasonal_test(x, test_full = FALSE,
#'                        p_limit_pass = 0.01, p_limit_warn = 0.05, 
#'                        robust_sa = FALSE, return_this = 'both')))
#' unemp_model <- lapply(unemp_seas_update, function(x) 
#'   try(model_test(x, return_this = 'both')))
#' unemp_sspan <- lapply(unemp_seas_update, function(x) 
#'   try(sspan_test(x, sf_limit = 15, change_limit = 35, 
#'       return_this = 'both')))
#' unemp_m7 <- lapply(unemp_seas_update, function(x) 
#'   try(mq_test(x, return_this = 'both')))
#' unemp_q2 <- lapply(unemp_seas_update, function(x) 
#'   try(mq_test(x, this_label = 'Q2', return_this = 'both')))
#' unemp_names <- names(unemployment_list)
#' unemp_diag_df <-
#'     make_diag_df(unemp_names, 
#'                  this_acf_test = unemp_acf, 
#'                  this_d11f_test = unemp_d11f, 
#'                  this_spec_peak_test = unemp_spec_peak, 
#'                  this_spec_peak_ori_test = unemp_spec_peak_ori,
#'                  this_qs_test = unemp_qs, 
#'                  this_qs_rsd_test = unemp_qs_rsd,
#'                  this_qs_seasonal_test = unemp_qs_seasonal, 
#'                  this_model_test = unemp_model,
#'                  this_sspan_test = unemp_sspan, 
#'                  this_m7_test = unemp_m7, 
#'                  this_q2_test = unemp_q2)
#' 
#' @export
make_diag_df <- function(this_data_names = NULL, this_acf_test = NULL, this_d11f_test = NULL, 
    this_spec_peak_test = NULL, this_spec_peak_ori_test = NULL, this_qs_test = NULL, 
    this_qs_rsd_test = NULL, this_qs_seasonal_test = NULL, this_model_test = NULL, 
    this_sspan_test = NULL, this_m7_test = NULL, this_q2_test = NULL, return_this = "both") {
    # Author: Brian C. Monsell (OEUS) Version 6.0, 5/21/2024
    
    # check if a value is specified for \code{this_data_names}
    if (is.null(this_data_names)) {
        stop("must specify a vector of time series names.")
    }

    # set number of names
    num_rows <- length(this_data_names)
    
    # Initialize overall diagnostic list
    all_diag_list <- list(n = 0, diag = 0, titles = 0)
    
    # add ACF test to data frame
    if (!is.null(this_acf_test)) {
        if (length(this_acf_test) < num_rows) {
            this_acf_test <- fix_diag_list(this_acf_test, this_data_names, return_this)
        }
        all_diag_list <- 
           update_diag_matrix(all_diag_list, this_acf_test, "ACF")
    }
    
    # add d11f test to data frame
    if (!is.null(this_d11f_test)) {
        if (length(this_d11f_test) < num_rows) {
            this_d11f_test <- fix_diag_list(this_d11f_test, this_data_names, return_this)
        }
        all_diag_list <- 
           update_diag_matrix(all_diag_list, this_d11f_test, "D 11F")
    }
    
    # add d11f test to data frame
    if (!is.null(this_spec_peak_test)) {
        if (length(this_spec_peak_test) < num_rows) {
            this_spec_peak_test <- fix_diag_list(this_spec_peak_test, this_data_names, return_this)
        }
        all_diag_list <- 
           update_diag_matrix(all_diag_list, this_spec_peak_test, "Spec Peaks (SA)")
    }
    
    # add qs test to data frame
    if (!is.null(this_qs_test)) {
        if (length(this_qs_test) < num_rows) {
            this_qs_test <- fix_diag_list(this_qs_test, this_data_names, return_this)
        }
        all_diag_list <- update_diag_matrix(all_diag_list, this_qs_test, "QS")
    }
    
    # add residual qs test to data frame
    if (!is.null(this_qs_rsd_test)) {
        if (length(this_qs_rsd_test) < num_rows) {
            this_qs_rsd_test <- fix_diag_list(this_qs_rsd_test, this_data_names, return_this)
        }
        all_diag_list <- update_diag_matrix(all_diag_list, this_qs_rsd_test, "QS(rsd)")
    }
    
    # add seasonal qs test to data frame
    if (!is.null(this_qs_seasonal_test)) {
        if (length(this_qs_seasonal_test) < num_rows) {
            this_qs_seasonal_test <- fix_diag_list(this_qs_seasonal_test, this_data_names, return_this)
        }
        all_diag_list <- 
           update_diag_matrix(all_diag_list, this_qs_seasonal_test, "QS(seasonal)")
    }
    
    # add spectrum test of original series
    if (!is.null(this_spec_peak_ori_test)) {
        if (length(this_spec_peak_ori_test) < num_rows) {
            this_spec_peak_test <- 
               fix_diag_list(this_spec_peak_ori_test, this_data_names, return_this)
        }
        all_diag_list <- 
           update_diag_matrix(all_diag_list, this_spec_peak_ori_test, "Spec Peaks (Ori)")
    }
    
    # add model test to data frame
    if (!is.null(this_model_test)) {
        if (length(this_model_test) < num_rows) {
            this_model_test <- fix_diag_list(this_model_test, this_data_names, return_this)
        }
        all_diag_list <- update_diag_matrix(all_diag_list, this_model_test, "Model")
    }
    
    # add sliding spans test to data frame
    if (!is.null(this_sspan_test)) {
        if (length(this_sspan_test) < num_rows) {
            this_sspan_test <- fix_diag_list(this_sspan_test, this_data_names, return_this)
        }
        all_diag_list <- update_diag_matrix(all_diag_list, this_sspan_test, "SSpan")
    }
    
    # add M7 test to data frame
    if (!is.null(this_m7_test)) {
        if (length(this_m7_test) < num_rows) {
            this_m7_test <- fix_diag_list(this_m7_test, this_data_names, return_this)
        }
        all_diag_list <- update_diag_matrix(all_diag_list, this_m7_test, "M7")
    }
    
    # add Q2 test to data frame
    if (!is.null(this_q2_test)) {
        if (length(this_q2_test) < num_rows) {
            this_q2_test <- fix_diag_list(this_q2_test, this_data_names, return_this)
        }
        all_diag_list <- update_diag_matrix(all_diag_list, this_q2_test, "Q2")
    }
    
    # add dimension names
    all_diag <- all_diag_list$diag
    dimnames(all_diag)[[1]] <- this_data_names
    dimnames(all_diag)[[2]] <- all_diag_list$titles
    
    # return matrix as a data frame
    return(data.frame(all_diag, stringsAsFactors = FALSE))
    
}
