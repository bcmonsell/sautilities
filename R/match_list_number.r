#' Number of list element matches
#'
#' Returns number of elements in list that matches \code{this_string}.
#'
#' Version 3.1, 5/21/2023
#'
#' @param this_list List of character strings.
#' @param this_string Character string to match against elements of the list, ie, 
#'        \code{this_string = 'pass'}. Default is \code{'fail'}.
#' @return The number of list items that match \code{this_string}. 
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' unemp_seas <- 
#'      seasonal::seas(unemployment_list, x11 = "", 
#'                     slidingspans = "", 
#'                     transform.function = "log",
#'                     arima.model = "(0 1 1)(0 1 1)",
#'                     forecast.maxlead = 60, 
#'                     check.print = c( "pacf", "pacfplot" ))
#' test_seas_update <- 
#'      Filter(function(x) inherits(x, "seas"), unemp_seas)
#' test_acf_test <- 
#'      lapply(test_seas_update, function(x) 
#'            try(acf_test(x, return_this = 'test')))
#' test_acf_number_fail <- 
#'      match_list_number(test_acf_test, 'fail')
#' test_acf_number_warn <- 
#'      match_list_number(test_acf_test, 'warn')
#' test_acf_number_pass <- 
#'      match_list_number(test_acf_test, 'pass')
#' @export
match_list_number <- function(this_list, this_string = "fail") {
    # Author: Brian C. Monsell (CSRM) Version 3.1, 5/21/2023
    
    # select names of list elements that match this_string
    this_list_names <- match_list(this_list, this_string)

    # return number of names matched
    this_list_number <- length(this_list_names)
    if (this_list_number == 1) {
        if (this_list_names[1] == "none") { 
            this_list_number <- 0
        }
    }

    return(this_list_number)
    
}
