#' Number of signifcant uregressors
#'
#' Determines the number of regressors in a \code{seas} object that are significant.
#'
#' Version 2.0, 5/1/2025
#'
#' @param seas_obj \code{seas} object generated from a call of \code{seas} 
#'        on a single time series. A required argument.
#' @param critical_value numeric scalar; sets the critical value used to test t-statistics of regressors.
#'        Default is 2.0
#' @param user_def_only Logical scalar; only test user-defined regressors
#'        Default is FALSE.
#' @return number of regressors with significant t-statistics
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' air_seas <- 
#'    seasonal::seas(AirPassengers, x11="", slidingspans = "", 
#'                   transform.function = "log", arima.model = "(0 1 1)(0 1 1)", 
#'                   regression.aictest = "td", forecast.maxlead=36, 
#'                   check.print = c( "pacf", "pacfplot" ), check.save = "acf")
#' air_num_user_reg <- significant_t_stats(air_seas)
#' @export
significant_t_stats <- function(seas_obj = NULL, critical_value = 2.0, user_def_only = FALSE) {
  # Author: Brian C. Monsell (OEUS) Version 2.0, 5/1/2025

    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
            stop("First argument must be a seas object")
        }
    }
	
	this_udg <- seasonal::udg(seas_obj)
	names_udg <- names(this_udg)
	nReg <- seasonal::udg(seas_obj, "nreg")
	if (nReg == 0) {
		return(0)
	}
	
	iReg <- sautilities::get_udg_index(this_udg, "nreg") + 1
	lReg <- iReg + nReg - 1
	
	iUser <- 0
	if (user_def_only) {
		nUser <- number_of_user_reg(seas_obj)
		if (nUser > 0) {
			for (i in iReg:lReg) {
				out <- stringi::stri_reverse(names_udg[i])
				out <- substr(out,1,5)
				out <- stringi::stri_reverse(out)
				if (out == "xreg1") {
					iUser <- i
				}
			}
			iReg <- iUser
			lReg <- iUser + nUser - 1
		} else {
			return(0)
		}
	}
	
	n_sig <- 0
	for (i in iReg:lReg) {
		this_t_stat <- abs(this_udg[[i]][3])
		if (this_t_stat > critical_value) {
			n_sig <- n_sig + 1
		}
	}
	
	return(n_sig)
	
}
