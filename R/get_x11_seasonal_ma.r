#' Get X-ll seasonal moving average
#'
#' Returns the seasonal moving average used in an X-ll seasonal adjustment from a seas object.
#'
#' Version 2.0, 2026-02-25
#'
#' @param seas_obj A \code{seas} object for a single series generated from the 
#'        \code{seasonal} package \insertCite{seasonal2018}{sautilities}. 
#'        This is a required entry.
#' @return Returns the seasonal moving average used in an X-ll seasonal adjustment from a seas object. 
#          Can be a single entry if the same moving average is used for all periods, or a vector for each period.
#'         Returns NULL if X-11 seasonal adjustment is not done.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples 
#' air_seas_short <- 
#'    seasonal::seas(AirPassengers, 
#'                   series.span = ',1960.9', 
#'                   transform.function = "log", 
#'                   arima.model="(0 1 1)(0 1 1)",
#'                   forecast.maxlead = 36, 
#'                   x11 = "", 
#'                   slidingspans = "")
#' air_sf_short  <- get_x11_seasonal_ma(air_seas_short)
#' @importFrom Rdpack reprompt
#' @export
get_x11_seasonal_ma <- function(seas_obj = NULL) {
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
	
	# get names from UDG output
	this_names <- names(seasonal::udg(seas_obj))
	
	# get MSR choice of seasonal if seasonalma = MSR
	if ("sfmsr" %in% this_names) {
		return(seasonal::udg(seas_obj, "sfmsr"))
	} else {
	# else get seasonalma value, if it exists
		this_freq <- seasonal::udg(seas_obj, "freq") 
		if ("seasonalma" %in% this_names) {
			this_sf <- as.vector(seasonal::udg(seas_obj, "seasonalma"))
			this_sum <- sum(this_sf == this_sf[1])
			if (this_sum == this_freq) {
				return(this_sf[1])
			} else {
				return(this_sf)
			}
		} else {
	# else stop and print out error message.
			stop("X-11 seasonal adjustment not done")
		}
	}

}