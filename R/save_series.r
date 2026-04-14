#' Save Series
#'
#' Save an R object with time series attributes (\code{ts})
#' to an external ASCII file in X-13ARIMA-SEATS' \code{datevalue} format.
#' For more information on the \code{datevalue} format of X-13ARIMA-SEATS,
#' see \insertCite{MonsellLytrasFindley2016;textual}{sautilities}.
#'
#' Version 2.0, 2026-03-03
#'
#' @param this_series double precision time series array to be saved.
#'        This is a required entry.
#' @param this_file character string; name of file time series array to be saved to.
#'        This is a required entry.
#' @return file with user-defined regressors will be produced
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
save_series <- function(this_series = NULL, this_file = NULL) {
    # Author: Brian C. Monsell (OEUS), Version 2.0, 2026-03-03
    
    # check if a value is specified for \code{this_series}
    if (is.null(this_series)) {
        stop("must specify a time series object.")
    }

    # check if a value is specified for \code{this_file}
    if (is.null(this_file)) {
        stop("must specify a valid file name.")
    }

    # generate time series data matrix with year, month, value
    temp <- cbind(time(this_series)%/%1, cycle(this_series), this_series)
    
    # save data matrix into \code{this_file}
    write.table(temp, this_file, sep = " ", row.names = FALSE, col.names = FALSE)
}
