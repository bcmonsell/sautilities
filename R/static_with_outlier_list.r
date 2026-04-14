#' Add outliers options and updated data to a list of \code{seas} object 
#'
#' Add outlier arguments and updated data to each element of a list of 
#' \code{seas} objects .
#' This function uses the \code{static} function of the \code{seasonal} pacakge; 
#' for more information on generating and evaluating a static seas object,  
#' see Section 8.1 of \insertCite{seasonal2018;textual}{sautilities}.
#'
#' Version 5.0, 2026-03-09
#'
#' @param seas_obj_list List object of \code{seas} objects generated from a call 
#'        of \code{seas} on a list consisting of individual time series.
#         Information on generating adjustments with the \code{seasonal} package 
#'        can be found in \insertCite{rprojectAdjustingMultiple;textual}{sautilities}. 
#'        This is a required entry.
#' @param new_data_list List object of time series (\code{ts})objects; 
#'        updated data sets from the data used to generate \code{seas_obj_list}. 
#'        This is a required entry.
#' @param outlier_span character string; sets the argument \code{outlier.span}.
#'        Default is \code{","}.
#' @param outlier_types character string; sets the argument \code{outlier.types}.
#'        Default is \code{"ao,ls"}.
#' @param outlier_method Character string; sets the argument \code{outlier.method}.
#'        Default is \code{"addone")}.
#' @param outlier_critical Character string; sets the argument \code{outlier.critica}.
#'        Default is Ljung choice for number of observations in \code{new_data}.
#'        \insertCite{Ljung93}{sautilities}.
#' @param cv_alpha Numeric scalar; alpha for critical value.
#'        Default is \code{0.01}.
#' @return A list of updated static seas object with outlier arguments included. 
#'         For more information about arguments to the \code{outlier} spec, 
#'         see Section 7.11 of \insertCite{Census2025;textual}{sautilities}.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' xt_seas_old <- 
#'    seasonal::seas(xt_data_old, 
#'       transform.function = "log", 
#'       forecast.maxlead = 60, 
#'       x11 = "")
#' xt_outlier_seas <- 
#'    static_with_outlier_list(xt_seas_old, 
#'       new_data_list = xt_data_new,
#'       outlier_types = "all",
#'       outlier_critical = 4.0) 
#' @importFrom Rdpack reprompt
#' @export
static_with_outlier_list <- 
   function(seas_obj_list = NULL, 
      new_data_list = NULL, 
      outlier_span = ",", 
	  outlier_types = "ao,ls",
	  outlier_method = "addone",
	  outlier_critical = NULL,
	  cv_alpha = 0.01) {
    # Author: Brian C. Monsell (OEUS) Version 5.0, 2026-03-09
    
    # check if a value is specified for \code{seas_obj_list}
    if (is.null(seas_obj_list)) {
        stop("must specify a list of seas objects")
    } else {
        if (!is.list(seas_obj_list)) {
            stop("must specify a list")
        }
    }
    
    # check if a value is specified for \code{new_data_list}
    if (is.null(new_data_list)) {
        stop("must specify an updated data set")
    } else {
        if (!is.list(new_data_list)) {
            stop("must specify a list")
        }
    }
    
    # set value for \code{outlier_critical}, if necessary
	if (is.null(outlier_critical)) {
		this_list_length <- 
		   max(unlist(lapply(new_data_list, length)))
		outlier_critical <- 
		   set_critical_value(this_list_length, cv_alpha)
	}

    seas_obj_list_update <- 
        Filter(function(x) inherits(x, "seas"), seas_obj_list)
        
    seas_obj_names <- names(seas_obj_list_update)
    num_names <- length(seas_obj_names)
    
    new_seas_list <- list()
    new_names_vec <- array(" ", dim = num_names)
    
    i2 <- 1
    for (i in 1:num_names) {
        this_name <- seas_obj_names[i]
        if (member_of_list(new_data_list, this_name)) {
            this_new_data <- new_data_list[[this_name]]
            new_seas_list[[i2]] <- 
                static_with_outlier(seas_obj = seas_obj_list[[i2]], 
				   new_data = this_new_data, 
				   outlier_span = outlier_span, 
                   outlier_types = outlier_types,
				   outlier_method = outlier_method,
				   outlier_critical = outlier_critical)
            new_names_vec[i2] <- this_name
            i2 <- i2 + 1
        } else {
            warning(paste0(this_name, " is not a member of the new data list"))
        }
    }
    
    names(new_seas_list) <- new_names_vec[1:(i2-1)]
    return(new_seas_list)
}
