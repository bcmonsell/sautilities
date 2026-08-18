#' Member of list
#'
#' Determines if a name is a member of a list.
#'
#' Version 1.5, 2026-04-16
#'
#' @param this_list A list of objects. This is a required entry.
#' @param this_name character string; element name of \code{this_list}.
#'        This is a required entry.
#' @return returns \code{TRUE} if \code{this_name} is an element of \code{this_list}, 
#'         \code{FALSE} otherwise.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' emp_seas_list <- 
#'    seasonal::seas(employment_list, 
#'       slidingspans = "", x11 = "",
#'       transform.function = "log",
#'       arima.model = "(0 1 1)(0 1 1)",
#'       forecast.maxlead=36,
#'       forecast.save = "fct",
#'       check.print = c( "pacf", "pacfplot" ))
#' emp_seas_update <- 
#'      Filter(function(x) inherits(x, "seas"), emp_seas_list)
#' if (member_of_list(emp_seas_update, 'n2000014')) {
#'     n2000014_fct <- seasonal::series(emp_seas_list$n2000014, "fct")
#' }
#' @export
member_of_list <- function(this_list = NULL, this_name = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 1.5, 2026-04-16

    # check if a value is specified for \code{this_list}
    if (is.null(this_list)) {
        stop("must specify a list of objects")
    } else {
        if (!is.list(this_list)) {
            stop("must specify a list")
        }
    }

    # check if a value is specified for \code{this_name}
    if (is.null(this_name)) {
        stop("must specify a character string")
    }

    return(this_name %in% names(this_list))
}
