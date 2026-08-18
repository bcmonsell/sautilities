#' Ljung Box Q Test failure message
#'
#' Tests whether the sample autocorrelation of the residuals from a time series model fails 
#' the Ljung-Box Q test \insertCite{LjungBox78}{sautilities}.
#'
#' Version 2.3, 2026-04-16
#'
#' @param this_acf Matrix object of the saved acf table.
#'        This is a required entry.
#' @param lbq_lags_fail Lags of the ACF to test.
#'        Default is \code{c(12, 24)}.
#' @param p_limit Numeric limit for the p-value of the Ljung-Box Q.
#'        Default is \code{0.01}.
#' @return Logical object which is \code{TRUE} if series fails the Ljung Box Q test, 
#'         \code{FALSE} otherwise.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' ukgas_seas <- 
#'    seasonal::seas(UKgas, 
#'       series.period = 4, 
#'       arima.model = "(0 1 1)(0 1 1)", 
#'       x11="", 
#'       transform.function = "log", 
#'       forecast.maxlead=20,
#'       check.save = 'acf', 
#'       check.print = c( "acf", "pacf", "pacfplot" ))
#' ukgas_acf <- seasonal::series(ukgas_seas, "acf")
#' ukgas_lbq_fail <- 
#'    lbq_fail(ukgas_acf, lbq_lags_fail = c(4, 8))
#' @importFrom Rdpack reprompt
#' @export
lbq_fail <- 
   function(this_acf = NULL, 
            lbq_lags_fail, 
            p_limit = 0.01) {
    # Author: Brian C. Monsell (OEUS) Version 2.3, 2026-04-16

    # check if a value is specified for \code{this_acf}
    if (is.null(this_acf)) {
        stop("must specify a matrix of ACF")
    } else {
        if (!is.matrix(this_acf)) {
            stop("must specify a matrix")
        }
    }
    
    # Initialize \code{this_fail} to \code{FALSE}
    this_fail <- FALSE
    
    # Extract the Ljung Box Q p-values from the ACF matrix
    this_lbq_pvalue <- vector(mode = "numeric", length = length(lbq_lags_fail)) 
    
    for (i in 1:length(lbq_lags_fail)) {
        this_lag <- lbq_lags_fail[i]
        this_lbq_pvalue[i] <- this_acf[this_lag,5]
        
        if (this_lbq_pvalue[i] < p_limit) {  this_fail <- TRUE  }
    }
    
    return(this_fail)
   
}