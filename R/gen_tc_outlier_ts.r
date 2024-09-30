#' Temporary change outlier regression as a \code{ts} object
#'
#' Generates a \code{ts} object for a TC (temporary change) outlier regressor
#'
#' Version 1.7, 5/25/2023
#'
#' @param tc_date Integer vector of length two - dates for TC outlier to be generated
#'        This is a required entry.
#' @param this_start Numeric vector; start date of TC outlier regressor generated.
#'        This is a required entry.
#' @param this_end Numeric vector; end date of TC outlier regressor generated.
#'        This is a required entry.
#' @param this_freq Numeric scalar; frequency of time series. 
#'        Default: \code{12}, for a monthly series
#' @param tc_alpha Numeric scalar; Rate of decay for the TC outlier. 
#'        Default: will be computed as in \code{X-13ARIMA-SEATS} for a weekly series
#' @param return_matrix Logical scalar; If true, the object returned is a one column  
#'        time series matrix object. Default: \code{TRUE}
#' @return \code{ts} object for a temporary change outlier regressor
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' UKgas_tc_date <- c(1970, 2)
#' UKgas_tc_1970_2 <-
#'    gen_tc_outlier_ts(UKgas_tc_date, this_start = c(1960, 1), this_end = c(1990, 4), 
#'                      this_freq = 4, tc_alpha = 0.9, return_matrix = TRUE)
#' @import stats
#' @export
gen_tc_outlier_ts <- function(tc_date = NULL, this_start = NULL, this_end = NULL, this_freq = 12,  
                              tc_alpha = NULL, return_matrix = TRUE) {
    # Author: Brian C. Monsell (OEUS), Version 1.7, 5/25/2023
    
    # check if a value is specified for \code{tc_date}
    if (is.null(tc_date)) {
        stop("must specify a date for the TC outlier")
    }
    
    # check if a value is specified for \code{this_start}
    if (is.null(this_start)) {
        stop("must specify a start date for the TC regressor")
    }
    
    # check if a value is specified for \code{this_end}
    if (is.null(this_end)) {
        stop("must specify an end date for the TC regressor")
    }
    
    # generate base vector, length of outlier regressors
    this_tc_outlier <- ts(0, start = this_start, end = this_end, frequency = this_freq)
    this_tsp <- tsp(this_tc_outlier)
    len_otlr <- length(this_tc_outlier)
    
    # If \code{tc_alpha} is not set, compute as in \code{X-13ARIMA-SEATS}
    if (is.null(tc_alpha)) {
        tc_alpha <- 0.7^(12/this_freq)
    }
    
    # generate year, period vectors for ts object
    this_year   <- time(this_tc_outlier) %/% 1
    this_period <- cycle(this_tc_outlier)
    this_filter <- (this_period == tc_date[2]) & (this_year == tc_date[1])
    if (sum(this_filter) == 0) {
       stop("tc_date not within span defined by this_start and this_end")
    }
        
    # Inititalize \code{this_obs} and \code{this_pos}
    this_obs <- 0.0
    this_pos <- len_otlr + 1
    
    for (j in 1:len_otlr) {
        # Update \code{this_obs} and \code{this_pos} at date of TC outlier for observation j
        if (this_filter[j]) {
            this_obs <- 1.0
            this_pos <- j
        }
        
        # Set observation j based on whether it is before or after the outlier date
        if (j > this_pos) {
            this_tc_outlier[j]  <- this_tc_outlier[j-1] * tc_alpha
        } else {
            this_tc_outlier[j]  <- this_obs
        }
    }
        
    # return outlier regressor
    
    if (return_matrix) {
        this_tc_outlier <- as.ts(matrix(this_tc_outlier, ncol = 1))
        tsp(this_tc_outlier) <- this_tsp
    }
    
    return(this_tc_outlier)
}
