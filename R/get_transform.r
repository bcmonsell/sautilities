#' Get transformation 
#'
#' Get transformation from the \code{seas} object of a single time series
#'
#' Version 2.0, 2026-02-25
#'
#' @param seas_obj A \code{seas} object for a single series generated from the 
#'        \code{seasonal} package \insertCite{seasonal2018}{sautilities}. 
#'        This is a required entry.
#' @return Character string with transformation used to model time series in 
#'         \code{seas} run
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' air_seas <- 
#'    seasonal::seas(AirPassengers, 
#'                   arima.model = "(0 1 1)(0 1 1)", 
#'                   x11="")
#' air_trans <- get_transform(air_seas)
#' @importFrom Rdpack reprompt
#' @export
get_transform <- function(seas_obj = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 2.0, 2026-02-25
    
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
