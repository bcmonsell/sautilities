#' Number of user defined regressors
#'
#' Extreact the number of user-defined regressors in a \code{seas} objects series.
#'
#' Version 2.0, 2026-02-26
#'
#' @param seas_obj A \code{seas} object for a single series generated from the 
#'        \code{seasonal} package \insertCite{seasonal2018}{sautilities}.
#'        This is a required entry.
#' @return number of user defined regressors in the model contained in \code{seas_obj}.
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
#'       check.save = "acf", 
#'       x11="", 
#'       slidingspans = "")
#' ukgas_num_user_reg <- number_of_user_reg(ukgas_seas)
#' @importFrom Rdpack reprompt
#' @export
number_of_user_reg <- function(seas_obj = NULL) {
  # Author: Brian C. Monsell (OEUS) Version 2.0, 2026-02-26

    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
            stop("First argument must be a seas object")
        }
    }

	this_user_reg <- seas_obj$spc$regression$user
	n_user_reg <- length(this_user_reg)
	
	return(n_user_reg)
}