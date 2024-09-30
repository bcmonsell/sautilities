#' QS Test for residual seasonality 
#'
#' Tests using the QS diagnostic developed by Maravall on seasonally adjusted series and the 
#' irregular component
#'
#' Version 4.4, 5/14/2024
#'
#' @param seas_obj \code{seas} object generated by the \code{seasonal} package.
#'        This is a required entry.
#' @param test_full Logical scalar indicating whether to test the full series span.
#'        Default is \code{TRUE}.
#' @param test_span Logical scalar indicating whether to test the final 8-year span used by the 
#'        spectrum diagnostic. Default is \code{TRUE}.
#' @param p_limit_fail Numeric scalar; P-value limit for QS statistic for failure.
#'        Default is \code{0.01}.
#' @param p_limit_warn Numeric scalar; P-value limit for QS statistic for warning.
#'        Default is \code{0.05}.
#' @param robust_sa Logical scalar indicating if SA or irregular series adjusted for extremes 
#'        is included in testing. Default is \code{TRUE}.
#' @param return_this character string; what the function returns - 
#'        \code{'test'} returns test results, \code{'why'} returns why the test failed or 
#'        received a warning, or \code{'both'}. Default is \code{'test'}.
#' @return A text string denoting if the series passed or failed tests for residual seasonality 
#'         using the QS diagnostics. Can test the entire series or the last 8 years or both.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' ukgas_seas <- 
#'    seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
#'                   x11="", transform.function = "log", forecast.maxlead=20,
#'                   check.print = c( "pacf", "pacfplot" ))
#' ukgas_qs_test <- qs_test(ukgas_seas, test_full = FALSE, p_limit_fail = 0.01, 
#'                          p_limit_warn = 0.05, return_this = 'both')
#' @export
qs_test <- function(seas_obj = NULL, test_full = TRUE, test_span = TRUE, p_limit_fail = 0.01, 
    p_limit_warn = 0.05, robust_sa = TRUE, return_this = "test") {
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
    udg_all <- seasonal::udg(seas_obj)
    
    # Check if full span of series has QS statistics for SA series and irregular.  
    # If none found, return \code{'no_qs_stats'}
    if (test_full) {
        args_all <- c("qssadj", "qssadjevadj", "qsirr", "qsirrevadj")
        n_test <- 0
        for (i in 1:4) {
            index_qs <- get_udg_index(udg_all, args_all[i])
            if (index_qs > 0) {
                n_test <- n_test + 1
            }
        }
        if (n_test == 0) {
            if (is_both) {
                return(c("no_qs_stats", "    "))
            } else {
                return("no_qs_stats")
            }
        }
    }
    
    # Check if full reduced span (8 years) of series has QS statistics for SA series and irregular.
    # If none found, return \code{'no_qs_stats'}
    if (test_span) {
        args_span <- c("qsssadj", "qsssadjevadj", "qssirr", "qssirrevadj")
        n_test <- 0
        for (i in 1:4) {
            index_qs <- get_udg_index(udg_all, args_span[i])
            if (index_qs > 0) {
                n_test <- n_test + 1
            }
        }
        if (n_test == 0) {
            if (is_both) {
                return(c("no_span_qs_stats", "    "))
            } else {
                return("no_span_qs_stats")
            }
        }
    }
    
    # initialize number of tests failed and number of warning messages to 0
    n_fail <- 0
    n_warn <- 0
        
    # test full span
    if (test_full) {
        # get index of the QS statistic for SA series
        index_qs <- get_udg_index(udg_all, "qssadj")
        
        # If this QS exists, update \code{n_warn} and \code{n_fail} if necessary
        if (index_qs > 0) {
            if (udg_all[["qssadj"]][2] < p_limit_fail) {
                n_fail <- n_fail + 1
            } else {
                if (udg_all[["qssadj"]][2] < p_limit_warn) {
                  n_warn <- n_warn + 1
                }
            }
        }
        
        # Test SA adjusted for extremes
        if (robust_sa) {
            # get index of the QS statistic for SA series adjusted for extremes
            index_qs <- get_udg_index(udg_all, "qssadjevadj")
            
            # If this QS exists, update \code{n_warn} and \code{n_fail} if necessary
            if (index_qs > 0) {
                if (udg_all[["qssadjevadj"]][2] < p_limit_fail) {
                  n_fail <- n_fail + 1
                } else {
                  if (udg_all[["qssadjevadj"]][2] < p_limit_warn) {
                    n_warn <- n_warn + 1
                  }
                }
            }
        }
        
        # get index of the QS statistic for irregular series
        index_qs <- get_udg_index(udg_all, "qsirr")
        
        # If this QS exists, update \code{n_warn} and \code{n_fail} if necessary
        if (index_qs > 0) {
            if (udg_all[["qsirr"]][2] < p_limit_fail) {
                n_fail <- n_fail + 1
            } else {
                if (udg_all[["qsirr"]][2] < p_limit_warn) {
                  n_warn <- n_warn + 1
                }
            }
        }
        
        # Test irregular series adjusted for extremes
        if (robust_sa) {
            # get index of the QS statistic for irregular series adjusted for extremes
            index_qs <- get_udg_index(udg_all, "qsirrevadj")
            if (index_qs > 0) {
                if (udg_all[["qsirrevadj"]][2] < p_limit_fail) {
                  n_fail <- n_fail + 1
                } else {
                  if (udg_all[["qsirrevadj"]][2] < p_limit_warn) {
                    n_warn <- n_warn + 1
                  }
                }
            }
        }
    }
    
    # initialize number of tests failed and number of warning messages for the reduced span to 0
    n_fail_span <- 0
    n_warn_span <- 0
        
    # test reduced (8-year) span
    if (test_span) {
        # get index of the QS statistic for SA series
        index_qs <- get_udg_index(udg_all, "qsssadj")
        
        # If this QS exists, update \code{n_warn_span} and \code{n_fail_span} if necessary
        if (index_qs > 0) {
            if (udg_all[["qsssadj"]][2] < p_limit_fail) {
                n_fail_span <- n_fail_span + 1
            } else {
                if (udg_all[["qsssadj"]][2] < p_limit_warn) {
                  n_warn_span <- n_warn_span + 1
                }
            }
        }
        
        # Test SA adjusted for extremes
        if (robust_sa) {
            # get index of the QS statistic for SA series adjusted for extremes
            index_qs <- get_udg_index(udg_all, "qsssadjevadj")
            
            # If this QS exists, update \code{n_warn_span} and \code{n_fail_span} 
            # if necessary
            if (index_qs > 0) {
                if (udg_all[["qsssadjevadj"]][2] < p_limit_fail) {
                  n_fail_span <- n_fail_span + 1
                } else {
                  if (udg_all[["qsssadjevadj"]][2] < p_limit_warn) {
                    n_warn_span <- n_warn_span + 1
                  }
                }
            }
        }
        
        # get index of the QS statistic for irregular series
        index_qs <- get_udg_index(udg_all, "qssirr")
        
        # If this QS exists, update \code{n_warn} and \code{n_fail} if necessary
        if (index_qs > 0) {
            if (udg_all[["qssirr"]][2] < p_limit_fail) {
                n_fail_span <- n_fail_span + 1
            } else {
                if (udg_all[["qssirr"]][2] < p_limit_warn) {
                  n_warn_span <- n_warn_span + 1
                }
            }
        }
        
        # Test SA adjusted for extremes
        if (robust_sa) {
            # get index of the QS statistic for irregular series adjusted for extremes
            index_qs <- get_udg_index(udg_all, "qssirrevadj")
            
            # If this QS exists, update \code{n_warn_span} and \code{n_fail_span} 
            # if necessary
            if (index_qs > 0) {
                if (udg_all[["qssirrevadj"]][2] < p_limit_fail) {
                  n_fail_span <- n_fail_span + 1
                } else {
                  if (udg_all[["qssirrevadj"]][2] < p_limit_warn) {
                    n_warn_span <- n_warn_span + 1
                  }
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
        this_why <- qs_fail_why(udg_all, test_full = is_fail, test_span = is_fail_span, 
            p_limit_fail = p_limit_fail, robust_sa = robust_sa, return_both = is_both)
    }
    if (is_warn | is_warn_span) {
        if (nchar(this_test) == 0) {
            this_test <- "warn"
        }
        this_why <- 
            paste0(this_why, qs_warn_why(udg_all, test_full = is_warn, test_span = is_warn_span, 
                   p_limit_warn = p_limit_warn, robust_sa = robust_sa, return_both = is_both))
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
    if (is_both) {
        return(cbind(this_test, this_why))
    }
    if (is_test) {
        return(this_test)
    }
    return(this_why)
    
}