#' Get transformation 
#'
#' Get transformation from the \code{seas} object of a single time series
#'
#' Version 1.10, 5/15/2024
#'
#' @param seas_obj \code{seas} object generated from a call of \code{seas} on a single time series 
#'        This is a required entry.
#' @return Character string with transformation used to model time series in \code{seas} run
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' air_seas <- seasonal::seas(AirPassengers, arima.model = "(0 1 1)(0 1 1)", x11="")
#' air_trans <- get_transform(air_seas)
#' @export
get_transform <- function(seas_obj = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 1.10, 5/15/2024
    
    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
           stop("First argument must be a seas object")
        }
    }

    # Get transformation information from UDG output
    this_trans <- seasonal::udg(seas_obj, "transform")
    
    # If automatic selection used, get result of AICC test from UDG output
    if (this_trans == "Automatic selection") {
        this_trans <- seasonal::udg(seas_obj, "aictrans")
    }
    
    return(this_trans)
}
