#' Update final t-statistics from outlier identification
#'
#' Update final t-statistics from automatic outlier identification with t-statistics from 
#' identified outliers in final regARIMA model. 
#' For more information on automatic outlier identification
#' in X-13ARIMA-SEATS, see Sectiona 4.6 and 7.11 of 
#' \insertCite{Census2025;textual}{sautilities}.
#'
#' Version 2.1, 2026-03-09
#'
#' @param seas_obj \code{seas} object generated from a call of \code{seas} 
#'        on a single time series \insertCite{seasonal2018}{sautilities}.
#'        This is a required entry.
#' @return Updated \code{fts} matrix.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' air_seas <- 
#'    seasonal::seas(AirPassengers, 
#'       arima.model = "(0 1 1)(0 1 1)",
#'       forecast.maxlead = 60, 
#'       forecast.save = "fct",
#'       outlier.save = "fts")
#' air_fts <- seasonal::series(air_seas, "fts")
#' air_fts_updated <- update_fts(air_seas)
#' @importFrom Rdpack reprompt
#' @export
update_fts <- function(seas_obj = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 2.1, 2026-03-09

	this_fts <- seasonal::series(seas_obj, "fts")
	
	this_auto_outlier <- sautilities::get_auto_outlier_string(seas_obj)
	n_auto_outlier    <- length(this_auto_outlier)
	
	if (n_auto_outlier > 0) {
		for (i in 1:n_auto_outlier) {
			this_key <- paste0("AutoOutlier$", this_auto_outlier[i])
			this_t   <- as.numeric(seasonal::udg(seas_obj, this_key))[3]
			this_otl <- sautilities::proc_outlier(this_auto_outlier[i])
		
			this_row_index <- 
				((time(this_fts) %/% 1) == this_otl$year) &
				(cycle(this_fts) == this_otl$period)
			
			this_col_label <- paste0("t.", toupper(this_otl$type), ".")
			this_col_index <- colnames(this_fts) == this_col_label
		
			this_fts[this_row_index, this_col_index] <- this_t
		}
	}

	return(this_fts)
}