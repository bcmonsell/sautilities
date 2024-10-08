#' Test X-11-ARIMA M and Q statistics
#'
#' Generates a test for X-11-ARIMA M and Q statistics.
#'
#' Version 3.3, 5/14/2024
#'
#' @param seas_obj Object generated by \code{seas()} of the seasonal package.
#'        This is a required entry.
#' @param this_label Character string; label for an M or Q statistic, such as \code{'M7'}, \code{'Q'}, 
#'        or \code{'Q2'}. Default is \code{'m7'}.
#' @param this_fail_limit Numeric scalar; value above which the M or Q statistic fails. 
#'        Default is \code{1.2}.
#' @param this_warn_limit Numeric scalar; value above which the M or Q statistic gives a 
#'        warning message if it is less than \code{this_fail_Limit}; default is \code{0.8}
#' @param return_this character string; what the function returns - 
#'        \code{'test'} returns test results, \code{'why'} returns why the test failed or 
#'        received a warning, or \code{'both'}. Default is \code{'test'}.
#' @return A text string denoting if series passes or has a warning for residual seasonality. 
#'         If \code{D11f} statistic not found, return \code{'none'}.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' ukgas_seas <- seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
#'                              x11="", transform.function = "log", forecast.maxlead=20,
#'                              check.print = c( "pacf", "pacfplot" ))
#' ukgas_q <- mq_test(ukgas_seas, this_label = 'q', return_this = 'both')
#' @export
mq_test <- function(seas_obj = NULL, this_label = "m7", this_fail_limit = 1.2,  
                    this_warn_limit = 0.8, return_this = "test") {
    # Author: Brian C. Monsell (OEUS) Version 3.3, 5/14/2024
    
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
    
    # get UDG key and extract quality control statistic from the UDG output
    this_mq_key <- get_mq_key(this_label)
    this_udg <- seasonal::udg(seas_obj)
    this_mq <- this_udg[[this_mq_key]]
    
    # if result of this_udg is \code{NULL}, return NULL
    if (is.null(this_mq)) { return(NULL) }
    
    # set \code{this_test}, \code{this_code} based on the different test limits
    if (this_mq < this_warn_limit) {
        this_test <- "pass"
        this_why  <- "    "
    } else {
        if (this_mq > this_fail_limit) {
            this_test <- "fail"
            this_why <- paste0(this_label, " > ", this_fail_limit)
        } else {
            this_test <- "warn"
            this_why <- paste0(this_warn_limit, " <= ", this_label, " <= ", this_fail_limit)
        }
    }
    
    # return \code{this_test} or \code{this_warn} based on value of \code{return_this}
    if (is_both) {
        return(cbind(this_test, this_why))
    }
    if (is_test) {
        return(this_test)
    }
    return(this_why)
    
}
