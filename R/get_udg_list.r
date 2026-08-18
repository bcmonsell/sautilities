#' Generate a list of \code{udg} elements from a list of \code{seas} objects.
#' 
#' Process a list of \code{seas} elements to get a list of results from the 
#' \code{udg} function of the \code{seasonal} package.
#' Information on generating adjustments of multiple time series stored in a
#' list object with the \code{seasonal} package can be found in 
#' \insertCite{rprojectAdjustingMultiple;textual}{sautilities}. 
#' 
#' Version 2.2, 2026-04-22
#'
#' @param seas_obj_list List object of \code{seas} objects generated from a call 
#'        of \code{seas} on a list consisting of individual time series.
#'        This is a required entry.
#' @param this_key Character string containing keyword of the \code{udg} function 
#'        that returns a numeric value. Default is \code{'autoout'}.
#' @return A list containing the results of extracting \code{this_key} from the \code{udg} output
#'         within the elements of \code{seas_obj_list}. 
#'         An error message will be printed if \code{try} errors are found.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples 
#' emp_seas_list <-
#'    seasonal::seas(unemployment_list, slidingspans = "", 
#'                   transform.function = "log", 
#'                   outlier.types = "all",
#'                   arima.model = "(0 1 1)(0 1 1)",
#'                   forecast.maxlead=36, x11 = "",
#'                   check.print = c( "pacf", "pacfplot" ))
#' emp_seas_update <- 
#'    Filter(function(x) inherits(x, "seas"), emp_seas_list)
#' xt_m7 <- 
#'   get_udg_list(emp_seas_update, this_key = 'f3.m07')
#' xt_q2 <- 
#'   get_udg_list(emp_seas_update, this_key = 'f3.qm2')
#' @importFrom Rdpack reprompt
#' @export
get_udg_list <- 
   function(seas_obj_list = NULL, 
            this_key = "autoout") {
    # Author: Brian C. Monsell (OEUS) Version 2.2, 2026-04-22
    
    # check if a value is specified for \code{this_seas_list}
    if (is.null(seas_obj_list)) {
        stop("must specify a list of seas objects")
    } else {
        if (!is.list(seas_obj_list)) {
            stop("must specify a list")
        }
    }
    
    # filter elements of the list that are not \code{seas} objects
    seas_obj_list_update <-
      Filter(function(x) inherits(x, "seas"), seas_obj_list)

    # extract diagnostic \code{this_key} from \code{seas_obj_list_update}
    udg_list <- lapply(seas_obj_list_update, function(x) try(seasonal::udg(x, this_key)))
    
    # check \code{udg_error} for try errors if try errors found, print out error messages and 
    # set \code{udg_error} to NULL
    udg_error <- which_error(udg_list)
    if (length(udg_error > 1)) {
        udg_list[[udg_error]] <- NULL
        print(paste("try errors found - ", udg_error))
    }
    
    # if there are no entries in \code{udg_error}, print error message and return NULL
    if (length(udg_list) == 0) {
        print("No series to process.")
        return(NULL)
    }
    
  udg_list <- lapply(udg_list, as.vector)
    
	return(udg_list)
	
}