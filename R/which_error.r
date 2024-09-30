#' Check list for try errors 
#'
#' Checks list for try errors, returning element names with errors 
#'
#' Version 2.9, 5/4/2024
#'
#' @param this_list list object which potentially contains \code{'try-error'} class objects.  
#' @return vector of the names of list elements that are \code{'try-error'} class objects. 
#'         If the list contains no \code{'try-error'} class objects, the function will 
#'         return \code{NULL}.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' unemp_seas_list <- 
#'   seasonal::seas(unemployment_list, slidingspans = "", 
#'                  transform.function = "log", forecast.maxlead = 36, 
#'                  arima.model = "(0 1 1)(0 1 1)",
#'                  check.print = c( "pacf", "pacfplot" ))
#' unemp_seas_update <- 
#'      Filter(function(x) inherits(x, "seas"), unemp_seas_list)
#' unemp_seas_errors <- which_error(unemp_seas_update) 
#' @import stats
#' @export
which_error <- function(this_list = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 2.9, 5/4/2024
    
    # check if a value is specified for \code{this_list}
    if (is.null(this_list)) {
        stop("must specify a list of try error objects")
    } else {
        if (!is.list(this_list)) {
            stop("must specify a list")
        }
    }
    
    # get names of elements with try-errors
    this_error <- lapply(this_list, function(x) try(inherits(x, "try-error")))
    this_error_names <- names(this_list)[unlist(this_error)]
    
    # return names or 'none' if no errors
    if (length(this_error_names) > 0) {
        return(this_error_names)
    } else {
        return(NULL)
    }
}
