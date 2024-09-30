#' Process list object of numbers
#'
#' Process list object of numbers and return names of elements that are either greater than or 
#' less than a limit.
#'
#' Version 2.2, 5/23/2023
#'
#' @param this_list List of numeric values. The elements should be scalars, not arrays.
#'        This is a required entry.
#' @param this_limit Numeric scalar which serves as the limit of the numbers stored in 
#'        \code{this_list}. This is a required entry.
#' @param abs_value Logical scalar that indicates whether the absolute value is taken of the  
#'        numbers before the comparison is made. (default is \code{FALSE})
#' @param greater_than logical object that specified whether the element names returned are  
#'        greater than or less than the limit specified in this_limit (default is \code{TRUE})
#' @return A vector of list element names where the value in \code{this_list} is greater than or 
#'         less than the limit specified in \code{this_limit}. If nothing matches, 
#'         the function will output the string \code{'none'}
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' emp_seas_list <- 
#'   seasonal::seas(employment_data_mts, 
#'                  slidingspans = "", forecast.maxlead=36, 
#'                  arima.model = "(0 1 1)(0 1 1)",
#'                  transform.function = "log", x11 = "",
#'                  check.print = c( "pacf", "pacfplot" ))
#' emp_seas_update <- 
#'     Filter(function(x) inherits(x, "seas"), emp_seas_list)
#' m7_key          <- get_mq_key('M7')
#' emp_m7_list     <- lapply(emp_seas_update, function(x) 
#'                           try(get_udg_entry(x, m7_key)))
#' emp_m7_pass     <- process_list(emp_m7_list, this_limit = 1.0, 
#'                           abs_value = TRUE, greater_than = FALSE)
#' @export
process_list <- function(this_list = NULL, this_limit = NULL, abs_value = FALSE, 
                         greater_than = TRUE) {
    # Author: Brian C. Monsell (OEUS) Version 2.2, 5/23/2023
    
    # check if a value is specified for \code{this_list}
    if (is.null(this_list)) {
        stop("must specify a list")
    } else {
        if (!is.list(this_list)) {
            stop("must specify a list")
        }
    }
    
    # check if a value is specified for \code{this_limit}
    if (is.null(this_limit)) {
        stop("must specify a limit")
    }
    
    # if \code{abs_value} is \code{TRUE}, return the element names on the list that are  
    # either greater than or less than in absolute value than \code{this_limit}
    if (abs_value) {
        if (greater_than) {
            this_name <- names(this_list)[abs(unlist(this_list)) > this_limit]
        } else {
            this_name <- names(this_list)[abs(unlist(this_list)) < this_limit]
        }
    } else {
        # else, return the element names on the list that are either greater than or less than 
        # \code{this_limit}
        if (greater_than) {
            this_name <- names(this_list)[unlist(this_list) > this_limit]
        } else {
            this_name <- names(this_list)[unlist(this_list) < this_limit]
        }
    }
    
    # return either the names or \code{'none'} if there are no names
    if (length(this_name) > 0) {
        return(this_name)
    } else {
        return("none")
    }
    
}
