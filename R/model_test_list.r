#' Tests Time Series Model.
#'
#' Tests whether a list of seas objects have time series models that have 
#' acceptable diagnostics.
#'
#' Version 1.2, 2026-04-17
#'
#' @param seas_obj_list A list of \code{seas} objects for a single series generated 
#'        from the \code{seasonal} package \insertCite{seasonal2018}{sautilities}.
#'        This is a required entry.
#' @param t_value t-statistic limit for regressors. 
#'        Default is \code{3}.
#' @param p_value p-value limit for regressors. 
#'        Default is \code{0.01}.
#' @param otl_auto_limit Numeric object; limit for number of automatically 
#'        identified outliers.
#'        Default is \code{4}.
#' @param otl_all_limit Numeric object; limit for number of outlier regressors.
#'        Default is \code{6}.
#' @param return_this character string; what the function returns - 
#'        \code{'test'} returns test results, 
#'        \code{'why'} returns why the test failed or received a warning, or 
#'        \code{'both'}. Default is \code{'both'}.
#' @param return_data_frame logical scalar; if \code{TRUE}, return a data frame 
#'        of the diagnostics, otherwise return a matrix. 
#'        Default is \code{FALSE}.
#' @return A list or data frame with text strings denoting if the each of the series 
#'         passed or failed the tests of ARIMA diagnostics.
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
#' unemp_model_df <- 
#'    model_test_list(unemp_seas_list, 
#'       t_value=3.0, 
#'       p_value=0.01, 
#'       otl_auto_limit=4, 
#'       otl_all_limit=6, 
#'       return_data_frame = TRUE)
#' @import stats
#' @importFrom Rdpack reprompt
#' @export
model_test_list <- 
   function(seas_obj_list = NULL, 
            t_value = 3, 
            p_value = 0.05, 
            otl_auto_limit = 5, 
            otl_all_limit = 5, 
            return_this = "test", 
            return_data_frame = FALSE) {
    # Author: Brian C. Monsell (OEUS) Version 1.2, 2026-04-17
    
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
	  
	  
	this_model_test_list <-
        lapply(seas_obj_list_update, function(x) try(model_test(x, 
            t_value = t_value, 
            p_value = p_value, 
            otl_auto_limit = otl_auto_limit, 
            otl_all_limit = otl_all_limit, 
            return_this = return_this)))

	if (return_data_frame) {
		this_series_name <- names(this_model_test_list)
		n_names <- length(this_series_name)
		
		if (return_this == "both") {
			this_matrix <- 
				matrix(unlist(this_model_test_list), 
				       nrow = n_names, byrow = TRUE)
			this_test <- this_matrix[,1]
			this_why  <- this_matrix[,2]
			this_df   <- data.frame(Test = this_test, Why = this_why)
		} else {
			this_matrix <- 
				matrix(unlist(this_model_test_list), 
				       nrow = n_names, ncol = 1)
			if (return_this == "test") {
				this_df <- data.frame(Test = this_matrix[,1])
			} else {
				this_df <- data.frame(Why = this_matrix[,1])
			}
		}
		
		rownames(this_df) <- this_series_name
		return(this_df)
	} else {
		return(this_model_test_list)
	}
}
