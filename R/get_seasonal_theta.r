#' Seasonal Moving Average from Airline Model
#'
#' Get the value of a seasonal moving average coefficient estimated from an airline model.
#'
#' Version 3.0, 5/14/2024
#'
#' @param seas_obj A \code{seas} object for a single series generated from the 
#'        \code{seasonal} package. This is a required entry.
#' @param freq A numeric scalar, the frequency of the time series. Default is 12.
#' @param this_index An integer scalar, an index of the vector values to be passed. 
#'        Acceptable values are \code{1} (seasonal MA coefficient value), 
#'        \code{2} (seasonal MA coefficent standard error), or 
#'        \code{3} (t-value of the Seasonal MA coefficient). Default is \code{1}.
#' @param return_string A Logical scalar; indicates whether value returned is a string or numeric. 
#'        Default is \code{TRUE}.
#' @param significant_digits an integer scalar; significant digits to be saved when a string 
#'        is returned. Default is \code{3}.
#' @return Character string containing a value related to the seasonal MA coefficient from the  
#'         regARIMA model fit in the \code{seas} object \code{seas_obj}. The standard error or 
#'         t-value of the seasonal MA coefficient can be returned depending on the value of  
#'         \code{this_index}. If \code{return_string} is \code{FALSE}, this is a numeric. 
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' air_seas <- seasonal::seas(AirPassengers, transform.function = "log", 
#'                         arima.model = "(0 1 1)(0 1 1)", x11="")
#' this_seasonal_theta <- get_seasonal_theta(air_seas, return_string = FALSE)
#' @export
get_seasonal_theta <- function(seas_obj = NULL, freq = 12, this_index = 1, return_string = TRUE, 
                               significant_digits = 3) {
    # Author: Brian C. Monsell (OEUS) Version 3.0, 5/14/2024
    
    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
        if (!inherits(seas_obj, "seas")) {
           stop("First argument must be a seas object")
        }
    }
    
    # Check for invalid entries of \code{freq}
    if (freq < 0) {
        stop("frequency cannot be less than 0.")
    }
    
    # Check for invalid entries of \code{this_index}
    if (this_index < 0) {
        stop("this_index cannot be less than 0.")
    }
    if (this_index > 3) {
        stop("this_index cannot be greater than 3.")
    }
    
    # Check for invalid entries of \code{significant_digits}
    if (significant_digits < 0) {
        stop("significant_digits cannot be less than 0.")
    }
    if (significant_digits > 8) {
        stop("significant_digits cannot be greater than 8.")
    }
    
    # generate UDG key for seasonal MA
    this_key <- paste("MA$Seasonal$", sprintf("%2.2i", freq), "$", sprintf("%2.2i", freq), sep = "")
    
    # extract seasonal MA coefficient if UDG keyword not found, print out error message and set
    # \code{this_seasonal} to NULL
    this_seasonal <- tryCatch(seasonal::udg(seas_obj, this_key), error = function(e) {
        print(paste("this keyword not found: ", this_key, sep = ""))
        NULL
    })
    
    # if \code{this_seasonal} is \code{NULL}, return \code{NULL} as value
    if (is.null(this_seasonal)) {
        return(this_seasonal)
    }
    
    # if \code{return_string} is \code{TRUE}, format output based on 
    # value of \code{significant_digits} else return numeric value
    if (return_string) {
        this_format <- paste("%", significant_digits + 4, ".", significant_digits, "f")
        this_seasonal_theta <- sprintf(this_format, this_seasonal[this_index])
    } else {
        this_seasonal_theta <- this_seasonal[this_index]
    }
    
    return(this_seasonal_theta)
}
