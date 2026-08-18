#' save a ts object into a csv file ready to be read by Python
#'
#' Save a \code{ts} object into a \code{.csv} file that is ready to be read by
#' Python.
#'
#' Version 1.4, 2026-07-01
#'
#' @param this_ts Double precision time series array to be saved.
#'        This is a required argument.
#' @param this_csv_file Character string; name of file time series array
#'        to be saved to. This is a required argument.
#' @param start_date Integer vector of length 2, Start date for series to be stored, 
#'        where the first element is the year and the second element is the month 
#'        or quarter. Default is start of series.
#' @return File with time series will be produced.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' UKgas_tc_date <- c(1970, 2)
#' UKgas_tc_1970_2 <-
#'    gen_tc_outlier_ts(UKgas_tc_date, 
#'       this_start = c(1960, 1), 
#'       this_end = c(1990, 4), 
#'       this_freq = 4, 
#'       tc_alpha = 0.9, 
#'       return_matrix = FALSE)
#' \dontrun{save_ts_to_csv(UKgas_tc_1970_2, this_csv_file = 'UKgas_tc_1970_2.csv')}
#' @export
save_ts_to_csv <- 
   function(this_ts = NULL, 
      this_csv_file = NULL, 
      start_date = NULL) {
    # Author: Brian C. Monsell (OEUS), Version 1.4, 2026-07-01
    
    if (is.null(this_ts)) {
        cat("must specify the time series regressor object")
        return(NULL)
    } else {
        # check if \code{this_ts} is a ts object
		if (!is.ts(this_ts)) {
			cat("must specify a ts object")
			return(NULL)
		}
    }    

    if (is.null(this_csv_file)) {
        cat("must specify the output file")
        return(NULL)
    } else {
		if (!is.character(this_csv_file)) {
			cat("must specify a character object for the csv file")
			return(NULL)
		}
    }

	if (!is.null(start_date)) {
		this_freq  <- frequency(this_ts)
		this_start <- start(this_ts)
		this_diff  <-
			difference_dates(start_date, this_start, this_freq)
		if (this_diff < 0) {
			cat(paste0("Date specified in start_date (", start_date, 
			           ") is before starting date of time series (", this_start))
			return(NULL)		   
		}
		this_ts <- window(this_ts, start = start_date)
	}
	
	this_period <- cycle(this_ts)
	this_year   <- floor(time(this_ts))
	
	this_time <- paste0(this_year, "-", sprintf("%02d", this_period), "-01")
	
	this_mat <- cbind(this_time, as.character(this_ts))
	colnames(this_mat) <- c("Date", "Value")
	
	write.csv(this_mat, file = this_csv_file, quote = FALSE, row.names = FALSE)
}