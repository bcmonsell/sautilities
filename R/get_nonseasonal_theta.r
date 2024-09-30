#' Nonseasonal Moving Average from Airline Model
#'
#' Get the value of a nonseasonal moving average coefficient estimated from an airline model.
#'
#' Version 1.10, May 14, 2024
#'
#' @param seas_obj A seas object for a single series generated from the \code{seasonal} package.
#'        This is a required entry.
#' @param this_index An integer scalar, an index of the vector values to be passed. 
#'        Acceptable values are \code{1} (nonseasonal MA coefficient value), 
#'        \code{2} (nonseasonal MA coefficent standard error), or 
#'        \code{3} (t-value of the nonseasonal MA coefficient). 
#'        Default is \code{1}.
#' @param return_string A Logical scalar; indicates whether value returned is a string or numeric. 
#'        Default is \code{TRUE}.
#' @param significant_digits an integer scalar; significant digits to be saved when a string 
#'        is returned. Default is \code{3}.
#' @return Character string containing a value related to the seasonal MA coefficient from the 
#'         regARIMA model fit in the \code{seas} object \code{seas_obj}. 
#'         If \code{return_string} is \code{FALSE}, this is a numeric. 
#'         The standard error or t-value of the seasonal MA coefficient can be returned depending 
#'         on the value of \code{this_index}.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' air_seas <- seasonal::seas(AirPassengers, transform.function = "log", 
#'                            arima.model = "(0 1 1)(0 1 1)", x11="")
#' this_nonseasonal_theta <- get_nonseasonal_theta(air_seas, return_string = FALSE)
#' @export
get_nonseasonal_theta <- function(seas_obj = NULL, this_index = 1, return_string = TRUE, 
                                  significant_digits = 3) {
    # Author: Brian C. Monsell (OEUS) Version 1.10, May 14, 2024
    
    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
           stop("First argument must be a seas object")
        }
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
    
    # generate UDG key for nonseasonal MA
    this_key <- paste("MA$Nonseasonal$", sprintf("%2.2i", 1), "$", sprintf("%2.2i", 1), sep = "")
    
    # extract nonseasonal MA coefficient if UDG keyword not found, print out error message and 
    # set \code{this_nonseasonal} to \code{NULL}
    this_nonseasonal <- tryCatch(seasonal::udg(seas_obj, this_key), error = function(e) {
        print(paste("this keyword not found: ", this_key, sep = ""))
        NULL
    })
    
    # if \code{this_nonseasonal} is \code{NULL}, return \code{NULL} as value
    if (is.null(this_nonseasonal)) {
        return(this_nonseasonal)
    }
    
    # if \code{return_string} is \code{TRUE}, format output based on value of 
    # \code{significant_digits} else return
    # numeric value
    if (return_string) {
        this_format <- paste("%", significant_digits + 4, ".", significant_digits, "f")
        this_nonseasonal_theta <- sprintf(this_format, this_nonseasonal[this_index])
    } else {
        this_nonseasonal_theta <- this_nonseasonal[this_index]
    }
    
    return(this_nonseasonal_theta)
}
