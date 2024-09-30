#' LBQ Test Explanation
#'
#' Ljung-Box Q Test Failure Message
#'
#' Generates text on why the sample autocorrelation of the residuals from a time series model 
#' fails the Ljung-Box Q test
#'
#' Version 1.11, 9/20/2023
#'
#' @param this_acf Matrix object of the saved acf table.
#'        This is a required entry.
#' @param lbq_lags_fail Lags of the ACF to test.
#'        Default is \code{c(12, 24)}.
#' @param p_limit - numeric limit for the p-value of the Ljung-Box Q.
#'        Default is \code{0.01}.
#' @param return_both Logical scalar indicating whether the calling function will return both the 
#'        test results and why the test failed or just produce a warning. Default is \code{FALSE}.
#' @return character object tells why series fails the Ljung-Box Q test, \code{'pass'} otherwise.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' ukgas_seas <- seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
#'                              x11="", transform.function = "log", forecast.maxlead=20,
#'                              check.maxlag = 12, check.save = 'acf')
#' ukgas_acf <- ukgas_seas$series$acf
#' ukgas_lbq_fail_why <- lbq_fail_why(ukgas_acf, lbq_lags_fail = c(4, 8), 
#'                                    return_both = TRUE)
#' @export
lbq_fail_why <- function(this_acf = NULL, lbq_lags_fail = c(12, 24), 
                         p_limit = 0.01, return_both = FALSE) {
    # Author: Brian C. Monsell (OEUS) Version 1.11, 9/20/2023
 
    # check if a value is specified for \code{this_acf}
    if (is.null(this_acf)) {
        stop("must specify a matrix of ACF")
    } else {
        if (!is.matrix(this_acf)) {
            stop("must specify a matrix")
        }
    }

    # Initialize \code{this_out_string}
    this_out_string <- ""
    this_lag_string <- ""
    
    # Extract the Ljung Box Q p-values from the ACF matrix
    
    for (i in 1:length(lbq_lags_fail)) {
        this_lag <- lbq_lags_fail[i]
        this_lbq_pvalue <- this_acf[this_lag,5]
        
        if (this_lbq_pvalue < p_limit) {  
            if (nchar(this_lag_string) > 0) {
                this_lag_string <- paste0(this_lag_string, ",", this_lag) 
            } else {
                this_lag_string <- paste0("(", this_lag)
            }
        }
    }
    
    if (nchar(this_lag_string) > 0) {  
        this_lag_string <- paste0(this_lag_string, ")")
        if (!return_both) {
            this_out_string <- "fail: "
        }
        this_out_string <- 
             paste0(this_out_string, " LBQ", this_lag_string, " sig at ", p_limit, " level")  
    }
    
    # if \code{this_out_string} has length 0, return pass, else return \code{this_out_string}
    if (nchar(this_out_string) == 0) {
        if (return_both) {
            return("    ")
        } else {
            return("pass")
        }
    } else {
        return(this_out_string)
    }
}
    
    