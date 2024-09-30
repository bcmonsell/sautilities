#' Fix Diagnostic List
#'
#' Fix an incomplete diagnostic list by filling in missing elements with NAs
#'
#' Version 1.6, 5/25/2023
#'
#' @param this_test list object of a seasonal adjustment or modeling diagnostic
#'        This is a required entry.
#' @param this_names character vector; complete set of names to check against
#'        This is a required entry.
#' @param return_this character string; what the function returns - 
#'        \code{'test'} returns test results, \code{'why'} returns why the test failed or 
#'        received a warning, or \code{'both'}. Default is \code{'both'}.
#' @return diagnostic list object with missing names filled in
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' xt_composite_seas <- seasonal::seas(xt_data_new,
#'      transform.function = "log", 
#'      check.print = c("none", "+acf", "+acfplot", "+normalitytest"), 
#'      regression.aictest = NULL, 
#'      outlier.types = "all",
#'      arima.model = "(0 1 1)(0 1 1)",
#'      list = list(
#'          list(x11 = ""),
#'          list(x11 = ""),
#'          list(seats.save = c("s11", "s12", "s10")),
#'          list(x11 = "")
#'      ),
#' )
#' xt_comp_update <- 
#'      Filter(function(x) inherits(x, "seas"), xt_composite_seas)
#' xt_test_m7 <- lapply(xt_comp_update, function(x) 
#'      try(mq_test(x, return_this = 'both')))
#' test_names <- names(xt_data_new)
#' num_names <- length(test_names)
#' if (!is.null(xt_test_m7)) {
#'     if (length(xt_test_m7) < num_names) {
#'       xt_test_m7 <- 
#'          fix_diag_list(xt_test_m7, test_names, return_this = 'both')
#'     }
#' }
#' @export
fix_diag_list <- function(this_test = NULL, this_names = NULL, return_this = "both") {
    # Author: Brian C. Monsell (OEUS) Version 1.6, 5/25/2023

    # check if a value is specified for \code{this_test}
    if (is.null(this_test)) {
        stop("must specify a diagnostic list object")
    }
    
    # check if a value is specified for \code{this_names}
    if (is.null(this_names)) {
        stop("must specify vector of names")
    } 
    
    # initialize \code{fixed_list}
    fixed_list <- list()

    # process each list element
    for (i in seq(length(this_names),1,-1)) {
        if (exists(this_names[i], where=this_test)) {
            # if name exists in list, save element into \code{fixed_list}
            fixed_list[[i]] <- this_test[[i]]
        } else {
            # if not, set element to NA or a 1x2 matrix of NAs
            if (return_this == "both") {
                fixed_list[[i]] <- matrix(c(NA,NA), nrow=1)
            } else {
                fixed_list[[i]] <- NA
            }
        }
    }

    # set names for the fixed list, and return
    names(fixed_list) <- this_names
    return(fixed_list)
}
