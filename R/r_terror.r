#' TERROR for R 
#'
#' A function that duplicates the functionality of the TERROR software (Caporello and Maravall 2004) 
#' that performs quality control on time series based on one step ahead forecasts 
#'
#' Version 3.9, 5/24/2023
#'
#' @param this_series Time series array. This is a required entry.
#' @param max_lead Number of forecasts generated by the seas run. Default is 36.
#' @param log_transform logical scalar, if TRUE \code{transform.function} will be set to \code{log} 
#         in the call to the \code{seas} function, otherwise \code{auto} will be used. 
#'        Default is \code{TRUE}.
#' @param aictest a character string with the entries for the \code{regression.aictest} argument 
#'        to the \code{seas} function from the \code{seasonal} package. 
#'        Default is \code{NULL}.
#' @param terror_lags Integer scalar for number of forecast lags from the end of series 
#'        we'll collect t-statistics. Must be either 1, 2, or 3.
#' @return t-statistics generated by out of sample forecast error for the last 1 to 3 
#'         observation of each series in the list.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples 
#' air_terror <- r_terror(AirPassengers, log_transform = TRUE,      
#'                        aictest = c('td', 'easter'), terror_lags = 3) 
#' @import stats
#' @export
r_terror <- function(this_series = NULL, max_lead = 36, log_transform = TRUE, aictest = NULL, 
                     terror_lags = 1) {
    # Author: Brian C. Monsell (OEUS) Version 3.9, 5/24/2023
    
    # check if a value is specified for \code{this_series}
    if (is.null(this_series)) {
        stop("must specify a time series")
    }
    
    # get ending date for \code{this_series}
    end_date <- end(this_series)
        
    # adjust ending date for the number of lags in the TERROR analysis
    end_date[2] <- end_date[2] - terror_lags
    if (end_date[2] < 0) {
        end_date[2] <- frequency(this_series) + end_date[2]
        end_date[1] <- end_date[1] - 1
    }
    
    # contruct span entry for analysis
    span_string <- paste(" ,", end_date[1], ".", end_date[2], sep = "")
    
    # set option for transformation
    if (log_transform) {
        trans_text <- "log"
    } else {
        trans_text <- "auto"
    }
    
    # run seasonal on list with automatic modeling
    this_seas <- seasonal::seas(this_series, transform.function = trans_text, 
        x11 = "", regression.aictest = aictest, forecast.maxlead = max_lead, 
        series.span = span_string)
    
    return(get_fcst_tval(this_seas, terror_lags))
        
}