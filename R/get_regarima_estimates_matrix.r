#' Generate summary of regARIMA model coefficients
#'
#' Generate a summary of coefficients from a regARIMA model for a single series.
#'
#' Version 3.0, 2026-02-25
#'
#' @param seas_obj A seas object for a single series generated from the 
#'        \code{seasonal} package \insertCite{seasonal2018}{sautilities}.
#'        This is a required entry.
#' @param add_diff logical scalar; add differencing information, if included in model
#' @param this_xreg_names Character array; name of user defined regressors.  
#'        Default is \code{NULL}, no user defined regressors. 
#'        Number of names in this vector should match number of user-defined regressors; 
#'        if not, a warning message will be produced.
#' @return A matrix of regARIMA model coefficients, standard errors, and t-statistics 
#'         for a given series.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' air_seas <- 
#'    seasonal::seas(AirPassengers, 
#'                   transform.function = "log", 
#'                   arima.model = "(0 1 1)(0 1 1)", 
#'                   forecast.maxlead=36, 
#'                   regression.aictest = 'td', 
#'                   check.print = c( "pacf", "pacfplot" ), 
#'                   x11 = "", slidingspans = "")
#' air_regarima_matrix <- get_regarima_estimates_matrix(air_seas)
#' @importFrom Rdpack reprompt
#' @export
get_regarima_estimates_matrix <- 
    function(seas_obj = NULL, 
	         add_diff = FALSE, 
	         this_xreg_names = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 3.0, 2026-02-25
    
    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
        if (!inherits(seas_obj, "seas")) {
           stop("First argument must be a seas object")
        }
    }
    
    # Generate matrix of the regression coefficients using \code{get_regression_estimates_matrix}
    this_regression_matrix <- 
        get_regression_estimates_matrix(seas_obj, this_xreg_names = this_xreg_names)
    
    # Generate matrix of the regression coefficients using \code{get_arima_estimates_matrix}
    this_arima_matrix <- 
        get_arima_estimates_matrix(seas_obj, add_diff = add_diff)
    
    # Generate a combined matrix of both regressors and ARIMA model coefficients
    if (is.null(this_regression_matrix)) {
        if (is.null(this_arima_matrix)) {
            return(NULL)
        } else {
            this_regarima_matrix <- this_arima_matrix
        }
    } else {
        if (is.null(this_arima_matrix)) {
            this_regarima_matrix <- this_regression_matrix
        } else {
            this_regarima_matrix <- rbind(this_regression_matrix, this_arima_matrix)
        }
    }
    
    # return model matrix
    return(this_regarima_matrix)
    
}
