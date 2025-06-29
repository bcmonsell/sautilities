#' Number of user defined regressors
#'
#' Extreact the number of user-defined regressors in a \code{seas} objects series.
#'
#' Version 1.0, 5/1/2025
#'
#' @param seas_obj \code{seas} object generated from a call of \code{seas} 
#'        on a single time series. A required argument.
#' @return number of user defined regressors in the model contained in seas_obj
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' air_seas <- 
#'    seasonal::seas(AirPassengers, x11="", slidingspans = "", 
#'                   transform.function = "log", arima.model = "(0 1 1)(0 1 1)", 
#'                   regression.aictest = "td", forecast.maxlead=36, 
#'                   check.print = c( "pacf", "pacfplot" ), check.save = "acf")
#' air_num_user_reg <- number_of_user_reg(air_seas)
#' @export
number_of_user_reg <- function(seas_obj = NULL) {
  # Author: Brian C. Monsell (OEUS) Version 1.0, 5/1/2025

    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
            stop("First argument must be a seas object")
        }
    }

	this_user_reg <- seas_obj$spc$regression$user
	n_user_reg <- length(this_user_reg)
	
	return(n_user_reg)
}