#' Update Diagnostic Matrix
#'
#' Update the matrix of diagnostics used to generate the diagnostic data frame 
#' in \code{make_diag_df}.
#'
#' Version 4.3, 5/24/2023
#'
#' @param this_diag_list list object with elements for seasonal adjustment or modeling diagnostic, 
#'        titles, and the number of columns. This is a required entry.
#' @param this_test_list list object of a specific seasonal adjustment or modeling diagnostic.
#'        This is a required entry.
#' @param this_label character string; name of diagnostic in \code{this_test_list}.
#'        This is a required entry.
#' @return list object with updated elements for seasonal adjustment or modeling diagnostic, 
#'         titles, and the number of columns.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' unemp_seas_list <- seasonal::seas(unemployment_list,
#'            x11 = "", slidingspans = "",
#'            arima.model = "(0 1 1)(0 1 1)",
#'            transform.function = "log",
#'            regression.aictest = NULL, 
#'            forecast.maxlead=60,
#'            check.print = c( "pacf", "pacfplot" ))
#' test_seas_update <- 
#'      Filter(function(x) inherits(x, "seas"), unemp_seas_list)
#' test_acf <- lapply(test_seas_update, function(x) try(acf_test(x, return_this = 'both')))
#' test_names <- names(xt_data_new)
#' num_names <- length(test_names)
#' all_diag_list <- list(n = 0, diag = 0, titles = 0)
#' if (!is.null(test_acf)) {
#'     if (length(test_acf) < num_names) {
#'         this_acf_test <- fix_diag_list(test_acf, test_names, return_this = 'both')
#'     }
#'     all_diag_list <- 
#'         update_diag_matrix(all_diag_list, test_acf, "ACF")
#' }
#' @export
update_diag_matrix <- function(this_diag_list = NULL, this_test_list = NULL, this_label = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 4.3, 5/24/2023
    
    # check if a value is specified for \code{this_diag_list}
    if (is.null(this_diag_list)) {
        stop("must specify a diagnostic list object")
    }
    
    # check if a value is specified for \code{this_test_list}
    if (is.null(this_test_list)) {
        stop("must specify a list object for a specific diagnostic")
    }    
    
    # check if a value is specified for \code{this_label}
    if (is.null(this_label)) {
        stop("must specify a label with the name of the diagnostic")
    }
    
    # grab names from \code{this_test_list}
    this_test_names <- names(this_test_list)
    
    # load tests into matrix
    this_test_matrix <- 
        matrix(unlist(this_test_list), nrow = length(this_test_list), 
                      byrow = TRUE)
    rownames(this_test_matrix) <- this_test_names
    
    # set diagnostic matrix and titles
    if (this_diag_list$n > 0) {
        this_diag_list$diag <- cbind(this_diag_list$diag, this_test_matrix)
        this_diag_list$titles <- c(this_diag_list$titles, this_label)
    } else {
        this_diag_list$diag <- this_test_matrix
        this_diag_list$titles <- this_label
    }
    if (ncol(this_test_matrix) > 1) {
        this_diag_list$titles <- c(this_diag_list$titles, paste(this_label, "fail?"))
    }
    
    # Update number of columns in data frame
    this_diag_list$n <- this_diag_list$n + ncol(this_test_matrix)
    
    return(this_diag_list)
}
