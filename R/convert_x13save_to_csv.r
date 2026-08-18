#' convert a \code{x13save} formatted file into a csv file ready to be read by Python
#'
#' Process a data file stored in \code{x13save} form and produce a \code{.csv} 
#' file that is ready to be read by Python. 
#' For more information on the \code{x13save} format of X-13ARIMA-SEATS,
#' see \insertCite{MonsellLytrasFindley2016;textual}{sautilities}.
#'
#' Version 1.6, 2026-07-01
#'
#' @param this_x13save_file Character string; name of file with a time series stored
#'        in \code{x13save} format. This is a required argument.
#' @param this_csv_file Character string; name of file time series array
#'        to be saved to. This is a required argument.
#' @param start_date Integer vector of length 2, Start date for series to be stored, 
#'        where the first element is the year and the second element is the month 
#'        or quarter. Default is start of series.
#' @return File with time series will be produced.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' \dontrun{save_series(UKgas, 'UKgas.dat', save_dv = FALSE)}
#' \dontrun{convert_x13save_to_csv('UKgas.dat', 'UKgas.csv')}
#' @importFrom Rdpack reprompt
#' @export
convert_x13save_to_csv <- 
   function(this_x13save_file = NULL, 
      this_csv_file = NULL, 
      start_date = NULL) {
    # Author: Brian C. Monsell (OEUS), Version 1.6, 2026-07-01
    
    if (is.null(this_x13save_file)) {
        cat("must specify the datevalue file")
        return(NULL)
    } else {
		if (!is.character(this_x13save_file)) {
			cat("must specify a character object for the x13save formatted file")
			return(NULL)
		}
    }

    if (is.null(this_csv_file)) {
        cat("must specify the output file")
        return(NULL)
    } else {
		if (!is.character(this_csv_file)) {
			cat("must specify a character object for the regression file")
				return(NULL)
		}
    }
	
	this_ts <- seasonal::import.ts(this_x13save_file, format = "x13save")

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