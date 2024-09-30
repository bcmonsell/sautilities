#' QS diagnostic for regarima residuals failure message
#'
#' Generates text explaining why the QS diagnostic failed or generated a warning for regARIMA residuals.
#'
#' Version 4.3, 5/25/2023
#'
#' @param udg_list List object generated by \code{udg()} function of the \code{seasonal} package.
#'        This is a required entry.
#' @param test_full Logical scalar indicating whether to test the full series span.
#'        Default is \code{TRUE}.
#' @param test_span Logical scalar indicating whether to test the final 8-year span used by 
#'        the spectrum diagnostic. Default is \code{TRUE}.
#' @param p_limit_fail Numeric scalar; P-value limit for QS statistic for warning.
#'        Default is \code{0.01}.
#' @param return_both Logical scalar indicating whether the calling function will return 
#'        both the test results and why the test failed or produced a warning. 
#'        Default is \code{FALSE}.
#' @return A text string denoting why the series failed the QS test of regARIMA residuals.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' ukgas_seas <- seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
#'                              x11="", transform.function = "log", forecast.maxlead=20,
#'                              check.print = c( "pacf", "pacfplot" ))
#' ukgas_udg <- seasonal::udg(ukgas_seas)
#' ukgas_qs_rsd_fail_why <- 
#'   qs_rsd_fail_why(ukgas_udg, test_full = FALSE, p_limit_fail = 0.005, return_both = TRUE)
#' @export
qs_rsd_fail_why <- function(udg_list = NULL, test_full = TRUE, test_span = TRUE,  
    p_limit_fail = 0.01, return_both = FALSE) {
    # Author: Brian C. Monsell (OEUS) Version 4.3, 5/25/2023
    
    # check if a value is specified for \code{udg_list}
    if (is.null(udg_list)) {
        stop("must specify a list of UDG diagnostics")
    } else {
        if (!is.list(udg_list)) {
            stop("must specify a list")
        }
    }
    
    # Initialize \code{this_out_string}
    this_out_string <- ""
    
    # Test full span of series
    if (test_full) {
        # initialize number of tests failed to 0
        n_test <- 0
        
        # get index of the QS statistic for the regARIMA residuals
        index_qs <- get_udg_index(udg_list, "qsrsd")
        
        # If this QS exists, check if the test fails, update \code{this_out_string} and 
        # \code{n_test} if necessary
        if (index_qs > 0) {
            if (udg_list[["qsrsd"]][2] < p_limit_fail) {
                this_out_string <- "rsd"
                if (!return_both) {
                  this_out_string <- paste("fail:", this_out_string, sep = " ")
                }
            }
            n_test <- n_test + 1
        }
        
        # If no QS statistics are found, return \code{'no_qs_stats'}
        if (n_test == 0) {
            return("no_qs_stats")
        }
    }
    
    # Test reduced (8-year) span of series
    if (test_span) {
        # initialize number of tests failed to 0
        n_test <- 0
        
        # get index of the QS statistic for the regARIMA residuals
        index_qs <- get_udg_index(udg_list, "qssrsd")
        
        # If this QS exists, check if the test fails, update \code{this_out_string} and 
        # \code{n_test} if necessary
        if (index_qs > 0) {
            if (udg_list[["qssrsd"]][2] < p_limit_fail) {
                if (nchar(this_out_string) > 0) {
                  this_out_string <- paste(this_out_string, ";rsd.span", sep = "")
                } else {
                  this_out_string <- "rsd.span"
                  if (!return_both) {
                    this_out_string <- paste("fail:", this_out_string, sep = " ")
                  }
                }
            }
            n_test <- n_test + 1
        }
        
        # If no QS statistics are found, return \code{'no_span_qs_stats'}
        if (n_test == 0) {
            return("no_span_qs_stats")
        }
    }
    
    # return results based on entry of \code{return_this}
    if (nchar(this_out_string) > 0) {
        return(this_out_string)
    } else {
        if (return_both) {
            return("    ")
        } else {
            return("pass")
        }
    }
    
}
