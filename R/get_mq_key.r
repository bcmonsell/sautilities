#' Make a UDG key for X-11-ARIMA M and Q statistics
#'
#' Generates the UDG key for \code{X-11-ARIMA} M and Q statistics based on a label
#'
#' Version 1.3, 05/24/2023
#'
#' @param this_label character string; name of an \code{X-11-ARIMA} M and Q statistics 
#'        This is a required entry.
#' @return character string with the corresponding UDG label for \code{this_label}. 
#'         If incorrect label is specified, returns \code{NULL}.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' m7_key  <- get_mq_key('M7')
#' @export
get_mq_key <- function(this_label = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 1.3, 05/24/2023
    
    # check if a value is specified for \code{this_label}
    if (is.null(this_label)) {
        stop("must specify the name of an X-11-ARIMA M and Q statistics")
    }
    # Initialize \code{this_key}
    this_key <- NULL
    
    # Use \code{switch} to set \code{this_key} based on the value of \code{this_label}
    switch(tolower(this_label), m1 = {
        this_key <- "f3.m01"
    }, m2 = {
        this_key <- "f3.m02"
    }, m3 = {
        this_key <- "f3.m03"
    }, m4 = {
        this_key <- "f3.m04"
    }, m5 = {
        this_key <- "f3.m05"
    }, m6 = {
        this_key <- "f3.m06"
    }, m7 = {
        this_key <- "f3.m07"
    }, m8 = {
        this_key <- "f3.m08"
    }, m9 = {
        this_key <- "f3.m09"
    }, m10 = {
        this_key <- "f3.m10"
    }, m11 = {
        this_key <- "f3.m11"
    }, q = {
        this_key <- "f3.q"
    }, q2 = {
        this_key <- "f3.qm2"
    })
    
    return(this_key)
}
