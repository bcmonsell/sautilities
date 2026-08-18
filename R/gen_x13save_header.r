#' Generate header for \code{x113save} formatted file
#'
#' Generate the header at the top of an X-13ARIMA-SEATS' \code{x13save} formatted file.
#' For more information on the \code{datevalue} format of X-13ARIMA-SEATS,
#' see \insertCite{MonsellLytrasFindley2016;textual}{sautilities}.
#'
#' Version 1.1, 2026-07-01
#'
#' @param this_series Time series array to be saved.
#'        This is a required entry.
#' @return Header for X-13ARIMA-SEATS \code{x13save} formatted file. Name taken for
#'         second column is taken from \code{this_series}.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' this_header <- gen_x13save_header(UKgas)
#' @importFrom Rdpack reprompt
#' @export
gen_x13save_header <- function(this_series) {
    # Author: Brian C. Monsell (OEUS), Version 1.1, 2026-07-01
	y <- deparse(substitute(this_series))
	this_tab <- "\t"
	this_line_end <- "\n"

	this_header <- 
		paste0("date", this_tab, y, this_line_end,
			"------", this_tab, "-----------------------")
	return(this_header)
}
