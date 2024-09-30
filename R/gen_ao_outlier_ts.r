#' Generate level change regression variable as a \code{ts} object
#'
#' Generates a \code{ts} object for a AO (point) outlier regressor.
#'
#' Version 1.7, 5/23/2023
#'
#' @param ao_date Integer vector of length two - dates for AO outlier to be generated.
#' @param this_start Numeric vector; start date of AO outlier regressor generated.
#' @param this_end Numeric vector; end date of AO outlier regressor generated.
#' @param this_freq Numeric scalar; frequency of time series. 
#'        Default: \code{12}, for a monthly series.
#' @param return_matrix Logical scalar; If true, the object returned is a one column time series matrix object. 
#'        Default: \code{TRUE}
#' @return A \code{ts} object of a point outlier regressor.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' UKgas_ao_date <- c(1970, 2)
#' UKgas_ao_1970_2 <-
#'    gen_ao_outlier_ts(UKgas_ao_date, this_start = c(1960, 1), this_end = c(1990, 4), 
#'                      this_freq = 4)
#' @import stats
#' @export
gen_ao_outlier_ts <- function(ao_date, this_start, this_end, this_freq = 12, return_matrix = TRUE) {
    # Author: Brian C. Monsell (OEUS), Version 1.7, 5/23/2023
    
    # Set number of outliers, length of outlier regressors
    this_ao_outlier <- ts(0, start = this_start, end = this_end, frequency = this_freq)
    this_tsp <- tsp(this_ao_outlier)
    len_otlr <- length(this_ao_outlier)
    
    # generate year, period vectors for ts object
    this_year   <- time(this_ao_outlier) %/% 1
    this_period <- cycle(this_ao_outlier)
    # create a filter indicating which observation matches the outlier date 
    this_filter <- (this_period == ao_date[2]) & (this_year == ao_date[1])
    if (sum(this_filter) == 0) {
       stop("ao_date not within span defined by this_start and this_end")
    }

    # set that observation to one in outlier variable
    this_ao_outlier[this_filter] <- 1
    
    if (return_matrix) {
        this_ao_outlier <- as.ts(matrix(this_ao_outlier, ncol = 1))
        tsp(this_ao_outlier) <- this_tsp
    }
        
    # return \code{this_ao_outlier}
    return(this_ao_outlier)
}
