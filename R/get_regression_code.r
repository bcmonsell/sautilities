#' Get X-13 entries for regression spec
#'
#' Get potential input for X-13ARIMA-SEATS for the \code{regression} spec for a given series.
#'
#' Version 1.1, 2026-04-22
#'
#' @param seas_obj A seas object for a single series generated from the 
#'        \code{seasonal} package \insertCite{seasonal2018}{sautilities}.
#'        This is a required entry.
#' @param this_argument Character string; argument from the X-13ARIMA-SEATS 
#'        \code{regression} spec. Possible values are \code{"variables"},  
#'        \code{"user"}, \code{"start"}, \code{"data"}, \code{"usertype"}, or 
#'        \code{"b"}. Default is \code{"variables"}.
#' @return Vector containing the entry for a \code{regression} argument from 
#'         \code{seas_obj}.  If there is no entry for that argument, 
#'         the routine will return a \code{NULL} object.
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
#'                   forecast.maxlead=36, 
#'                   check.print = c( "pacf", "pacfplot" ), 
#'                   x11 = "", slidingspans = "")
#' air_reg_var <- get_regression_code(air_seas)
#' air_reg_b   <- get_regression_code(air_seas, "b")
#' @importFrom Rdpack reprompt
#' @export
get_regression_code <- function(seas_obj = NULL, this_argument = "variables") {
    # Author: Brian C. Monsell (OEUS) Version 4.1, 2026-04-22

    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
           stop("First argument must be a seas object")
        }
    }

    arg_set <- c("variables", "user", "start", "data", "usertype", "b")
	good_arg <- this_argument %in% arg_set
	if (!good_arg) {
        cat("must specify one of the following for this_argument:")
		stop(cat(paste(dQuote(arg_set), collapse = " ")))
	}
	
	this_arg_list <- seas_obj$model$regression
	return(as.vector(unlist(this_arg_list[this_argument])))

}