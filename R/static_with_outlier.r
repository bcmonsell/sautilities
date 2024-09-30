#' Add outliers to \code{seas} object 
#'
#' Add arguments from the \code{outlier} spec to a \code{seas} object.
#'
#' Version 2.9, 5/14/2024
#'
#' @param seas_obj \code{seasonal} object. This is a required entry.
#' @param new_data time series object; updated data set from the data used to generate 
#'        \code{seas_obj}. This is a required entry.
#' @param outlier_span character string; sets the argument \code{outlier.span}.
#'        Default is \code{",")}.
#' @param outlier_types character string; sets the argument \code{outlier.types}.
#'        Default is \code{"ao,ls")}.
#' @return An updated static \code{seas} object with \code{outlier} arguments included.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' shoes_seas <- 
#'    seasonal::seas(shoes2007, slidingspans = "", transform.function = "log", x11 = "",
#'                    forecast.maxlead = 60)
#' shoes_seas_outlier <- 
#'    static_with_outlier(shoes_seas, shoes2008, outlier_types = 'all')
#' @import stats
#' @export
static_with_outlier <- function(seas_obj = NULL, new_data = NULL, outlier_span = ",", 
                                outlier_types = "ao,ls") {
    # Author: Brian C. Monsell (OEUS) Version 2.9, 5/14/2024
    
    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
           stop("First argument must be a seas object")
        }
    }
    
    # check if a value is specified for \code{new_data}
    if (is.null(new_data)) {
        stop("must specify an updated data set")
    }
    
    # save static version of \code{seas} object
    static_seas_object <- seasonal::static(seas_obj, evaluate = TRUE)
    
    # update static version of object with updated data, \code{outlier} arguments
    static_seas_object_outlier <- 
        update(static_seas_object, x = new_data, outlier.span = outlier_span, 
               outlier.types = outlier_types)
    
    # return static version of object
    return(static_seas_object_outlier)
}
