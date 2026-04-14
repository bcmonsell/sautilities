#' List element match
#'
#' Returns element of list that matches \code{this_string}.
#'
#' Version 4.2, 2025-09-11
#'
#' @param this_list List of character strings.
#' @param this_string Character string to match against elements of the list, ie, 
#'        \code{this_string = 'pass'}. Default is \code{'fail'}.
#' @param match_substring Logical scalar; if \code{TRUE}, the function will consider a match to be
#'        a match of any substring in the string. The default is \code{FALSE}, where the string must
#'        match the entire string to be considered a match.
#' @return A vector of list element names that match \code{this_string}. 
#'         Users can specify that the whole string matches, or that a substring matches.
#'         If nothing matches, the function will output the string \code{'none'}.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' unemp_seas <- 
#'    seasonal::seas(unemployment_list, x11 = "", 
#'       slidingspans = "", 
#'       transform.function = "log",
#'       arima.model = "(0 1 1)(0 1 1)",
#'       forecast.maxlead = 60, 
#'       check.print = c( "pacf", "pacfplot" ))
#' test_seas_update <- 
#'    Filter(function(x) inherits(x, "seas"), unemp_seas)
#' test_acf_test <- lapply(test_seas_update, function(x) 
#'      try(acf_test(x, return_this = 'test')))
#' test_acf_fail <- match_list(test_acf_test, 'fail')
#' test_acf_warn <- match_list(test_acf_test, 'warn')
#' test_acf_pass <- match_list(test_acf_test, 'pass')
#' @export
match_list <- 
     function(this_list, this_string = "fail", match_substring = FALSE) {
    # Author: Brian C. Monsell (CSRM) Version 4.2, 2025-09-11
    
    # select names of list elements that match this_string
	if (match_substring) {
		this_list_names <- names(this_list)[grepl(this_string, unlist(this_list), fixed = TRUE)]
	} else {
		this_list_names <- names(this_list)[unlist(this_list) == this_string]
	}
    
    # return vector of names or 'none' if no elements match the string
    if (length(this_list_names) > 0) {
        return(this_list_names)
    } else {
        return("none")
    }
}
