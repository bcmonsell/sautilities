#' Get transformation 
#'
#' Get transformation from the \code{seas} object of a single time series.
#'
#' Version 3.1, 2026-04-21
#'
#' @param seas_obj A \code{seas} object for a single series generated from the 
#'        \code{seasonal} package \insertCite{seasonal2018}{sautilities}. 
#'        This is a required entry.
#' @param return_code logical scalar; if \code{TRUE}, return input for X-13.
#'        Default is \code{TRUE}.
#' @return Character string with transformation used to model time series in 
#'         \code{seas} run.
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
#' air_trans_label <- 
#'    get_transform(air_seas, return_code = FALSE)
#' @importFrom Rdpack reprompt
#' @export
get_transform <- function(seas_obj = NULL, return_code = TRUE) {
    # Author: Brian C. Monsell (OEUS) Version 3.1, 2026-04-21
    
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
	this_trans <- 
		get_udg_entry(seas_obj, 
		              this_key = "transform", 
					  this_index = -1, 
					  return_numeric = FALSE)
    
    # If automatic selection used, get result of AICC test from UDG output
	if (is.null(this_trans)) {
	    stop("The transform key doesn't exist in the seasonal object")
	} else {
	    if (this_trans == "Automatic selection") {
		    this_trans <- 
			    get_udg_entry(seas_obj, 
				              "aictrans", 
					          this_index = -1, 
					          return_numeric = FALSE)
		}
	}
	
    # If return_code is TRUE, convert this_trans to output that can be used
	# as input for X-13
	if (return_code) {
	    if (this_trans == "Log(y)") {
			return("log")
		}
	    if (this_trans == "No transformation") {
			return("none")
		}
	    if (this_trans == "log(y/(1-y))") {
			return("logistic")
		}
	    if (this_trans == "sqrt(y)") {
			return("sqrt")
		}
		return(substr(this_trans, 1, 5))
	}
	
	return(this_trans)
}
