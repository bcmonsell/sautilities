#' Ljung-Box Q ACF test
#'
#' Tests whether the residuals from a time series model has acceptable autocorrelation as indicated
#! by the Ljung-Box Q test.
#'
#' Version 1.7, 5/3/2024
#'
#' @param seas_obj Object generated by \code{seas()} of the \code{seasonal} package.
#'        This is a required entry.
#' @param lbq_lags_fail - lags of the ACF to test
#'        Default is \code{c(12, 24)}.
#' @param p_limit - numeric limit for the p-value of the Ljung-Box Q
#'        Default is \code{0.01}.
#' @param return_this character string; what the function returns - 
#'        \code{'test'} returns test results, \code{'why'} returns why the test failed or 
#'        received a warning, or \code{'both'}. Default is \code{'test'}.
#' @return A text string denoting if series passes, fails, or has a warning for residual 
#'         autocorrelation. If Ljung-Box not found, the seasonal object will be updated with
#'         check spec arguments - if this is unsuccessful, return \code{'none'}.
#'         If regARIMA model not estimated, return \code{'none'}
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' ukgas_seas <- 
#'    seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
#'                   x11="", transform.function = "log", forecast.maxlead=20,
#'                   check.save = 'acf', check.maxlag = 12, check.qlimit = 0.01)
#' ukgas_lbq_test <- lbq_test(ukgas_seas, lbq_lags_fail = c(4, 8), return_this = 'both')
#' @export
lbq_test <- function(seas_obj = NULL, lbq_lags_fail = c(12, 24), p_limit = 0.01, 
                     return_this = "test") {
    # Author: Brian C. Monsell (OEUS) Version 1.7, 5/3/2024

    
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
    
    this_udg <- seasonal::udg(seas_obj, "mdg")
    this_acf <- seasonal::series(seas_obj, "acf")
    
    # check to see if a regARIMA model was estimated in this run.  
    # If not, return \code{'none'}
    if (this_udg[["mdg"]] == "no") {
        if (is_both) {
            return(c("none","    "))
        } else {
            return("none")
        }
    } else {
    # if acf table doesn't exist, update the seas object to add the acf table
        if (is.null(this_acf)) {
            seas_obj <- 
                update(seas_obj, check.qlimit = p_limit, check.save = "acf", 
                       check.maxlag = max(lbq_lags_fail)) 
            this_acf <- seasonal::series(seas_obj, "acf")
        }
    }
    
    this_fail <- lbq_fail(this_acf, lbq_lags_fail, p_limit)
    if (this_fail) {
        this_test <- "fail"
        this_why <- lbq_fail_why(this_acf, lbq_lags_fail, p_limit, 
                                 return_both = is_both)
    } else {
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