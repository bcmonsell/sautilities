#' First overall sasonality test from \insertCite{Maravall2012;textual}{sautilities}
#' applied to a list.
#'
#' Conduct the first overall test for seasonality as laid out in 
#' \insertCite{Maravall2012;textual}{sautilities} applied to a list of 
#' seasonal objects.
#' Information on generating adjustments of multiple time series stored in a
#' list object with the \code{seasonal} package can be found in 
#' \insertCite{rprojectAdjustingMultiple;textual}{sautilities}. 
#'
#' Version 1.4, 2026-04-16
#'
#' @param seas_obj_list A list of \code{seas} objects for a single series generated 
#'        from the \code{seasonal} package \insertCite{seasonal2018}{sautilities}.
#'        This is a required entry.
#' @param this_series Character string; the table used to generate the AR(30) 
#'        spectrum. 
#'        Default is \code{"a1"}.
#' @param take_log Logical scalar; indicates if the AR spectrum is generated 
#'        from the log of the data. 
#'        Default is \code{TRUE}.
#' @param this_ori_qs Character string; code for which original series QS will 
#'        be used in this analysis. 
#'        Default is \code{"qsori"} (original series, full span); other choices are  
#'        \code{"qssorievadj"} (original series adjusted for extreme values, 
#'        short span), 
#'        \code{"qsorievadj"} (original series adjusted for extreme values, full span), and 
#'        \code{"qssori"} (original series, short span).
#'        For more information on the QS statistic, 
#'        see \insertCite{Bell2022;textual}{sautilities}.
#' @param return_data_frame Logical scalar; if \code{TRUE}, return a data frame 
#'        of the diagnostics, otherwise return a matrix. 
#'        Default is \code{FALSE}.
#' @return A list or data frame of seasonality diagnostics for a set of series.
#'         Each object will contain 3 elements: \code{QStest} (test probability 
#'         for QS), \code{NPtest} (test probability for NP),
#'         and \code{result} (character string with test result - 
#'         possible values of either \code{"evidence of seasonality"} and 
#'         \code{"no evidence of seasonality"}).
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' unemp_seas_list <- 
#'    seasonal::seas(unemployment_list, slidingspans = "", 
#'                   transform.function = "log", 
#'                   outlier.types = "all",
#'                   arima.model = "(0 1 1)(0 1 1)",
#'                   forecast.maxlead=36, x11 = "",
#'                   check.print = c( "pacf", "pacfplot" ), 
#'                   check.save = "acf",
#'                   series.save = "a1")
#' unemp_seas_update <- 
#'      Filter(function(x) inherits(x, "seas"), unemp_seas_list)
#' first_test_df <- 
#'      overall_seasonal_test_1_list(unemp_seas_update, 
#'                                   return_data_frame = TRUE)
#' @importFrom Rdpack reprompt
#' @export
overall_seasonal_test_1_list <- 
    function(seas_obj_list = NULL, 
	         this_series = "a1", 
			 take_log = TRUE, 
	         this_ori_qs = "qsori", 
             return_data_frame = FALSE) {
    # Author: Brian C. Monsell (OEUS) Version 1.4, 2026-04-16

    # check if a value is specified for \code{seas_obj_list}
    if (is.null(seas_obj_list)) {
        stop("must specify a list of seasonal objects")
    } else {
        if (!is.list(seas_obj_list)) {
            stop("must specify a list")
        }
    }
   
   # filter elements of the list that are not seas objects
    seas_obj_list_update <-
      Filter(function(x) inherits(x, "seas"), seas_obj_list)
	  
	this_overall_test_one <- 
        lapply(seas_obj_list_update, function(x) try(overall_seasonal_test_1(x, 
            this_series = this_series, 
			take_log = take_log, 
	        this_ori_qs = this_ori_qs, 
            return_data_frame = TRUE)))

    if (return_data_frame) {
		this_series_name <- names(this_overall_test_one)
		n_names <- length(this_series_name)
		this_matrix <- 
		    matrix(unlist(this_overall_test_one), 
			       nrow = n_names, byrow = TRUE)
		this_QS <- as.numeric(this_matrix[,1])
		this_NP <- as.numeric(this_matrix[,2])
		this_df <- data.frame(QStest = this_QS, NPtest = this_NP, 
		                      Result = this_matrix[,3])
		rownames(this_df) <- this_series_name
		return(this_df)
	} else {
		return(this_overall_test_one)
	}
}
