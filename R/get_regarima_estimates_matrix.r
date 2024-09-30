#' Generate summary of regARIMA model coefficients
#'
#' Generate a summary of coefficients from a regARIMA model for a single series 
#'
#' Version 2.5, 5/14/2024
#'
#' @param seas_obj \code{seas} object generated from a call of \code{seas} on a single time series 
#'        This is a required entry.
#' @param add_diff logical scalar; add differencing information, if included in model
#' @param this_xreg_names Character array; name of user defined regressors.  
#'        Default is \code{NULL}, no user defined regressors. 
#'        Number of names in this vector should match number of user-defined regressors; 
#'        if not, a warning message will be produced.
#' @return A matrix of regARIMA model coefficients, standard errors, and t-statistics 
#'         for a given series
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' air_seas <- seasonal::seas(AirPassengers, x11="", slidingspans = "", 
#'                            transform.function = "log", arima.model = "(0 1 1)(0 1 1)", 
#'                            regression.aictest = 'td', forecast.maxlead=36, 
#'                            check.print = c( "pacf", "pacfplot" ))
#' air_regarima_matrix <- get_regarima_estimates_matrix(air_seas)
#' @export
get_regarima_estimates_matrix <- 
    function(seas_obj = NULL, add_diff = FALSE, this_xreg_names = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 2.5, 5/14/2024
    
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
