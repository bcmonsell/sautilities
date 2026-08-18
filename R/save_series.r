#' Save Series
#'
#' Save an R object with time series attributes (\code{ts})
#' to an external ASCII file in X-13ARIMA-SEATS' \code{datevalue} format
#' or X-13ARIMA-SEATS' \code{datevalue} format.
#' For more information on the \code{datevalue} format of X-13ARIMA-SEATS,
#' see \insertCite{MonsellLytrasFindley2016;textual}{sautilities}.
#'
#' Version 3.0, 2026-07-01
#'
#' @param this_series Time series array to be saved.
#'        This is a required entry.
#' @param this_file Character string; name of file time series array to be saved to.
#'        This is a required entry.
#' @param save_dv Logical scalar; if \code{TRUE}, time series object is stored in
#'        \code{datevalue} format; else, the time series object is stored in 
#'        \code{x13save} format.
#' @return File with time series data in the specified format will be produced.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' ukgas_seas <-
#'    seasonal::seas(UKgas, 
#'       series.period = 4, 
#'       transform.function = "log", 
#'       arima.model = "(0 1 1)(0 1 1)", 
#'       forecast.maxlead = 20,
#'       check.print = c( "pacf", "pacfplot" ),
#'       x11 = "")
#' ukgas_sa <- seasonal::final(ukgas_seas)
#' \dontrun{save_series(ukgas_sa, 'ukgas_sa.txt')}
#' @import stats
#' @import utils
#' @importFrom Rdpack reprompt
#' @export
save_series <- function(this_series = NULL, this_file = NULL, save_dv = TRUE) {
    # Author: Brian C. Monsell (OEUS), Version 3.0, 2026-07-01
	this_name <- deparse(substitute(this_series))
    
    # check if a value is specified for \code{this_series}
    if (is.null(this_series)) {
        stop("must specify a time series object.")
    }

    # check if a value is specified for \code{this_file}
    if (is.null(this_file)) {
        stop("must specify a valid file name.")
    }

	# save series in \code{datevalue} format
	if (save_dv) {
		# generate time series data matrix with year, month, value
		temp <- cbind(time(this_series)%/%1, cycle(this_series), this_series)
    
		# save data matrix into \code{this_file}
		write.table(temp, 
			this_file, 
			sep = " ", 
			row.names = FALSE, 
			col.names = FALSE)
	} else {
		# generate header for \code{x13save} format
		this_tab <- "\t"
		this_header <- gen_x13save_header(this_name)
		
		# generate time series data matrix with year-period, value
		year_period <- (time(this_series)%/%1) * 100 + cycle(this_series)
		this_value <- formatC(this_series, format = "e", digits = 15, flag = "+")
		
		temp <- cbind(as.character(year_period), as.vector(this_value))
		
		# save data matrix into \code{this_file}
		write.table(temp, this_file, 
			sep = this_tab, 
			row.names = FALSE, 
			col.names = FALSE,
		    append = TRUE,
		    quote = FALSE)
	}
}
