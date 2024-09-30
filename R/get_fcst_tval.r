#' T-values of within sample forecasts 
#'
#' Returns t-values of within sample forecasts, up to 3 
#'
#' Version 2.10, 5/14/2024
#'
#' @param seas_obj \code{seas} object for a single series
#'        This is a required entry.
#' @param terror_lags Integer scalar for number of forecast lags from the end of series we'll 
#'        collect t-statistics. Must be either 1, 2, or 3.
#'        This is a required entry.
#' @return A numeric array of t-values of within sample forecasts, up to length 3.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples 
#' air_seas_short <- seasonal::seas(AirPassengers, series.span = ',1960.9', 
#'                     arima.model="(0 1 1)(0 1 1)", x11 = "",
#'                     forecast.maxlead = 36, slidingspans = "", 
#'                     transform.function = "log")
#' fcst_tstat <- get_fcst_tval(air_seas_short, 3)
#' @export
get_fcst_tval <- function(seas_obj = NULL, terror_lags = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 2.10, 5/14/2024
    
    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
           stop("First argument must be a seas object")
        }
    }
    
    # check if a value is specified for \code{terror_lags}
    if (is.null(terror_lags)) {
        stop("must specify a lag for the TERROR analysis")
    }
    
    # Initialize vector for forecast t-values
    fcst_tval_vec <- array(0, dim = terror_lags)
    
    # for each lag, generate a key and extract the t-value from the UDG output
    for (i in 1:terror_lags) {
        this_key <- paste("forctval0", i, sep = "")
        fcst_tval_vec[i] <- get_udg_entry(seas_obj, this_key)
    }
    
    return(fcst_tval_vec)
}
