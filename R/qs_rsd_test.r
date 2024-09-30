#' QS diagnostic test
#'
#' Tests using the QS diagnostic developed by Maravall
#'
#' Version 4.4, 5/14/2024
#'
#' @param seas_obj \code{seas} object generated by the \code{seasonal} package.
#'        This is a required entry.
#' @param test_full Logical scalar indicating whether to test the full series span.
#'        Default is \code{TRUE}.
#' @param test_span Logical scalar indicating whether to test the final 8-year span used 
#'        by the spectrum diagnostic. Default is \code{TRUE}.
#' @param p_limit_fail Numeric scalar; P-value limit for QS statistic for failure.
#'        Default is \code{0.01}.
#' @param p_limit_warn Numeric scalar; P-value limit for QS statistic for warning.
#'        Default is \code{0.05}.
#' @param return_this character string; what the function returns - 
#'        \code{'test'} returns test results, \code{'why'} returns why the test failed or 
#'        received a warning, or \code{'both'}. Default is \code{'test'}.
#' @return A text string denoting if the regarima residuals passed or failed tests for 
#'         residual seasonality using the QS diagnostics. Can test the entire series or the 
#'         last 8 years or both. 
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' ukgas_seas <- seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
#'                              x11="", transform.function = "log", forecast.maxlead=20,
#'                              check.print = c( "pacf", "pacfplot" ))
#' ukgas_qs_test_rsd <- qs_rsd_test(ukgas_seas, test_full = FALSE, p_limit_fail = 0.01, 
#'                                  p_limit_warn = 0.05, return_this = 'both')
#' @export
qs_rsd_test <- function(seas_obj = NULL, test_full = TRUE, test_span = TRUE,  
    p_limit_fail = 0.01, p_limit_warn = 0.05, return_this = "test") {
    # Author: Brian C. Monsell (OEUS) Version 4.4, 5/14/2024
    
    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
           stop("First argument must be a seas object")
        }
    }
    
    # set \code{is_test}, \code{is_why}, \code{is_both}
    is_test <- FALSE
    is_why <- FALSE
    is_both <- FALSE
    return_this <- tolower(return_this)
    if (return_this == "test") {
        is_test <- TRUE
    }
    if (return_this == "why") {
        is_why <- TRUE
    }
    if (return_this == "both") {
        is_test <- TRUE
        is_why <- TRUE
        is_both <- TRUE
    }
    
    # check for improper entry for \code{return_this}
    if (!is_test & !is_why) {
        print(paste("Improper entry for return_this : ", return_this, sep = ""))
        return(NULL)
    }
    
    # generate UDG list of diagnostics
    udg_list <- seasonal::udg(seas_obj)
    
    # Check if full span of series has QS statistics for residuals.  If none found, return
    # \code{'no_qsrsd_stats'}
    if (test_full) {
        
        index_qs <- get_udg_index(udg_list, "qsrsd")
        if (index_qs == 0) {
            if (is_both) {
                return(c("no_qsrsd_stats", "    "))
            } else {
                return("no_qsrsd_stats")
            }
        }
    }
    
    # Check if reduced (8-year) span of series has QS statistics for residuals.  If none found, return
    # \code{'no_span_qsrsd_stats'}
    if (test_span) {
        index_qs <- get_udg_index(udg_list, "qssrsd")
        if (index_qs == 0) {
            if (is_both) {
                return(c("no_span_qsrsd_stats", "    "))
            } else {
                return("no_span_qsrsd_stats")
            }
        }
    }
    
    # initialize number of tests failed and number of warning messages to 0
    n_fail <- 0
    n_warn <- 0
        
    # test full span
    if (test_full) {
        # get index of the QS statistic for the regARIMA residuals
        index_qs <- get_udg_index(udg_list, "qsrsd")
        
        # If this QS exists, update \code{n_warn} and \{n_fail} if necessary
        if (index_qs > 0) {
            if (udg_list[["qsrsd"]][2] <= p_limit_warn) {
                if (udg_list[["qsrsd"]][2] > p_limit_fail) {
                  n_warn <- n_warn + 1
                } else {
                  n_fail <- n_fail + 1
                }
            }
        }
        
    }
    
    # initialize number of tests failed and number of warning messages to 0
    n_fail_span <- 0
    n_warn_span <- 0
        
    # test reduced (8-year) span
    if (test_span) {
        # get index of the QS statistic for the regARIMA residuals
        index_qs <- get_udg_index(udg_list, "qssrsd")
        
        # If this QS exists, update \code{n_warn_span} and \code{n_fail_span} if necessary
        if (index_qs > 0) {
            if (udg_list[["qssrsd"]][2] <= p_limit_warn) {
                if (udg_list[["qssrsd"]][2] > p_limit_fail) {
                  n_warn_span <- n_warn_span + 1
                } else {
                  n_fail_span <- n_fail_span + 1
                }
            }
        }
        
    }
    
    # Generate indicator variables for whether the QS test failed or should generate a warning
    is_fail <- n_fail > 0
    is_fail_span <- n_fail_span > 0
    is_warn <- n_warn > 0 & n_fail == 0
    is_warn_span <- n_warn_span > 0 & n_fail_span == 0
    
    # Initialize error and warning message text strings
    this_test <- ""
    this_why <- ""
    
    # Generate error and warning message text strings, if necessary
    if (is_fail | is_fail_span) {
        this_test <- "fail"
        this_why <- 
            qs_fail_why(udg_list, test_full = is_fail, test_span = is_fail_span, 
                       p_limit_fail = p_limit_fail, return_both = is_both)
    }
    if (is_warn | is_warn_span) {
        if (nchar(this_test) == 0) {
            this_test <- "warn"
        }
        this_why <- 
            paste(this_why, qs_warn_why(udg_list, test_full = is_warn, test_span = is_warn_span, 
                  p_limit_warn = p_limit_warn, return_both = is_both), sep = "")
    }
    
    # If no error and warning messages generated, indicate test passes
    if (n_fail + n_warn + n_fail_span + n_warn_span == 0) {
        this_test <- "pass"
        this_why <- "    "
        if (return_this == "why") {
            this_why <- "pass"
        }
    }
    
    # return results based on entry of \code{return_this}
    if (return_this == "both") {
        return(cbind(this_test, this_why))
    }
    if (is_test) {
        return(this_test)
    }
    return(this_why)
}
