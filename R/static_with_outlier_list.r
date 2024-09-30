#' Add outliers to list of \code{seas} object 
#'
#' Add outlier arguments to each element of a list of \code{seas} objects .
#'
#' Version 3.4, 5/25/2023
#'
#' @param seas_obj_list list of seasonal objects. This is a required entry.
#' @param new_data_list list of time series objects; updated data sets from the data used 
#'        to generate \code{seas_obj_list}. This is a required entry.
#' @param outlier_span character string; sets the argument \code{outlier.span}.
#'        Default is \code{","}.
#' @param outlier_types character string; sets the argument \code{outlier.types}.
#'        Default is \code{"ao,ls"}.
#' @return A list of updated static seas object with outlier arguments included.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' xt_seas_old <- 
#'    seasonal::seas(xt_data_old, slidingspans = "", 
#'                   transform.function = "log", 
#'                   x11 = "", forecast.maxlead = 60)
#' xt_outlier_seas <- 
#'    static_with_outlier_list(xt_seas_old, xt_data_new) 
#' @export
static_with_outlier_list <- function(seas_obj_list = NULL, new_data_list = NULL, 
                                     outlier_span = ",", outlier_types = "ao,ls") {
    # Author: Brian C. Monsell (OEUS) Version 3.4, 5/25/2023
    
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
                static_with_outlier(seas_obj_list[[i2]], this_new_data, outlier_span, 
                                    outlier_types)
            new_names_vec[i2] <- this_name
            i2 <- i2 + 1
        } else {
            warning(paste0(this_name, " is not a member of the new data list"))
        }
    }
    
    names(new_seas_list) <- new_names_vec[1:(i2-1)]
    return(new_seas_list)
}
