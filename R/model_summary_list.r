#' ARIMA model summary from a list
#'
#' Generate a matrix that summarizes ARIMA models from a list of \code{seas} objects.  
#'
#' Version 2.1, 5/22/2024
#'
#' @param seas_obj_list list of \code{seas} objects generated from a call of \code{seas} 
#'        on a single time series. A required argument.
#' @param add_diff logical scalar; add differencing information, if included in model
#' @param return_data_frame logical scalar; if \code{TRUE}, return a data frame of the diagnostics, 
#'        otherwise return a matrix. Default is \code{FALSE}.
#' @return matrix of ARMA model orders, nonseasonal and seasonal total parameters 
#'         for a given set of series
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' unemp_seas_list <- 
#'    seasonal::seas(unemployment_list, slidingspans = "", 
#'                   transform.function = "log", 
#'                   outlier.types = "all",
#'                   arima.model = "(0 1 1)(0 1 1)",
#'                   forecast.maxlead=36, x11 = "",
#'                   check.print = c( "pacf", "pacfplot" ))
#' unemp_seas_update <- 
#'      Filter(function(x) inherits(x, "seas"), unemp_seas_list)
#' unemp_model_matrix <- 
#'     model_summary_list(unemp_seas_update)
#' unemp_model_df <- 
#'     model_summary_list(unemp_seas_update, add_diff = TRUE, 
#'                        return_data_frame = TRUE)
#' @export
model_summary_list <- function(seas_obj_list = NULL, add_diff = FALSE, 
                               return_data_frame = FALSE)
{
    # Author: Brian C. Monsell (OEUS) Version 2.1, 5/22/2024
    
    # check if a value is specified for \code{seas_obj_list}
    if (is.null(seas_obj_list)) {
        stop("must specify a list of seasonal objects")
    } else {
        if (!is.list(seas_obj_list)) {
            stop("must specify a list")
        }
    }
    
    # filter elements of the list that are not seas objects
    seas_obj_list_update <-
        Filter(function(x) inherits(x, "seas"), seas_obj_list)

    # if differences are included in summary, get number of seasonal 
    # and nonseasonal differences
    if (add_diff) {
        this_diff <- 
            lapply(seas_obj_list_update, function(x) 
                   try(seasonal::udg(x, "nonseasonaldiff")))
        this_sdiff <- 
            lapply(seas_obj_list_update, function(x) 
                   try(seasonal::udg(x, "seasonaldiff")))
        this_colname <-
            c("Nonseasonal AR", "Nonseasonal MA", "Seasonal AR", "Seasonal MA",
              "Total ARMA", "Total Nonseasonal", "Total Seasonal", 
              "Diff Nonseasonal", "Diff Seasonal")
    } else {
        this_colname <-
            c("Nonseasonal AR", "Nonseasonal MA", "Seasonal AR", "Seasonal MA",
              "Total ARMA", "Total Nonseasonal", "Total Seasonal")
    }
    
    this_udg <- 
        lapply(seas_obj_list_update, function(x) 
               try(seasonal::udg(x)))
                   
    this_nmodel <- 
        lapply(seas_obj_list_update, function(x) 
               try(seasonal::udg(x, "nmodel")))
        
    this_index <-
        lapply(this_udg, function(x) 
                   try(sautilities::get_udg_index(x, "nmodel")))
                   
    this_names <- names(this_index)
    num_names <- length(this_names)
    
    ma_nonseasonal    <- array(rep(0, num_names), dim = num_names)            
    ar_nonseasonal    <- array(rep(0, num_names), dim = num_names)
    ma_seasonal       <- array(rep(0, num_names), dim = num_names)
    ar_seasonal       <- array(rep(0, num_names), dim = num_names)
    total_nonseasonal <- array(rep(0, num_names), dim = num_names)
    total_seasonal    <- array(rep(0, num_names), dim = num_names)

    for (i in 1:num_names) {
        that_name   <- this_names[i]
        that_index  <- this_index[[that_name]]
        that_nmodel <- this_nmodel[[that_name]]
        that_udg    <- this_udg[[that_name]]
        if (that_nmodel > 0) {
            for (j in 1:that_nmodel) {
                that_key <- names(that_udg)[that_index + j]
                that_arma <- 
                    unlist(strsplit(that_key, split = '[$]'), 
                           use.names = FALSE)
            
                if (that_arma[2] == "Nonseasonal") {
                    if (that_arma[1] == "MA") {
                        ma_nonseasonal[i] <- ma_nonseasonal[i] + 1
                    } else {
                        ar_nonseasonal[i] <- ar_nonseasonal[i] + 1
                    }
                }
                if (that_arma[2] == "Seasonal") {
                    if (that_arma[1] == "MA") {
                        ma_seasonal[i] <- ma_seasonal[i] + 1
                    } else {
                        ar_seasonal[i] <- ar_seasonal[i] + 1
                    }
                }
                total_nonseasonal[i] <- ma_nonseasonal[i] + ar_nonseasonal[i]
                total_seasonal[i]    <- ma_seasonal[i] + ar_seasonal[i]
            }
        }
    }
    
    this_matrix <- 
        cbind(ar_nonseasonal, ma_nonseasonal, ar_seasonal, ma_seasonal, 
              unlist(this_nmodel, use.names = FALSE),
              total_nonseasonal, total_seasonal)
              
    if (add_diff) {
        this_matrix <- 
            cbind(this_matrix,
                  unlist(this_diff, use.names = FALSE),
                  unlist(this_sdiff, use.names = FALSE))
    }
    colnames(this_matrix) <- this_colname
    rownames(this_matrix) <- this_names
    
    if (return_data_frame) {
        return(data.frame(this_matrix))
    } else {    
        return(this_matrix)
    }
    
}
