#' Level change regression variable as a \code{ts} object
#'
#' Generates a ts object for a LS (level shift) outlier regressor.
#'
#' Version 1.8, 5/23/2023
#'
#' @param ls_date Integer vector of length two - dates for LS outlier to be generated.
#' @param this_start Numeric vector; start date of LS outlier regressor generated.
#' @param this_end Numeric vector; end date of LS outlier regressor generated.
#' @param this_freq Numeric scalar; frequency of time series. 
#'        Default: \code{12}, for a monthly series
#' @param x13type Logical scalar; Indicates if level change outlier is defined as in X-13ARIMA-SEATS. 
#'        Default: \code{TRUE}
#' @param return_matrix Logical scalar; If true, the object returned is a one column time series 
#'        matrix object. Default: \code{TRUE}
#' @return Generate \code{ts} object of a level change outlier regressor
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' UKgas_ls_date <- c(1970, 2)
#' UKgas_ls_1970_2 <-
#'    gen_ls_outlier_ts(UKgas_ls_date, this_start = c(1960, 1), this_end = c(1990, 4), 
#'                      this_freq = 4)
#' @import stats
#' @export
gen_ls_outlier_ts <- function(ls_date, this_start, this_end, this_freq = 12, x13type = TRUE, 
                              return_matrix = TRUE) {
    # Author: Brian C. Monsell (OEUS), Version 1.8, 5/23/2023
    
    # Set number of outliers, length of outlier regressors
    this_ls_outlier <- ts(0, start = this_start, end = this_end, frequency = this_freq)
    this_tsp <- tsp(this_ls_outlier)
    len_otlr <- length(this_ls_outlier)
    
    # generate year, period vectors for ts object
    this_year   <- time(this_ls_outlier) %/% 1
    this_period <- cycle(this_ls_outlier)
    this_filter <- (this_period == ls_date[2]) & (this_year == ls_date[1])
    if (sum(this_filter) == 0) {
       stop("ls_date not within span defined by this_start and this_end")
    }
    
    if (this_filter[1]) {
        stop("ls_date cannot be the same as this_start.")
    }

    # Format outlier regressor to maintain the level of the series for the most recent observations
    if (x13type) {
        # set constant used to set the current observation to -1
        this_obs <- -1
        
        for (j in 1:len_otlr) {
            # change constant to 0 when filter matches current observation
            if (this_filter[j]) {
              this_obs <- 0
            }
            this_ls_outlier[j] <- this_obs
        }
    } else {
        # set constant used to set the current observation to 0
        this_obs <- 0

        for (j in 1:len_otlr) {
            # change constant to 1 when filter matches current observation
            if (this_filter[j]) {
              this_obs <- 1
            }
            this_ls_outlier[j] <- this_obs
        }
    }
    
    if (return_matrix) {
       this_ls_outlier <- as.ts(matrix(this_ls_outlier, ncol = 1))
       tsp(this_ls_outlier) <- this_tsp
    }
        
    # return \code{this_ls_outlier}
    return(this_ls_outlier)
}
