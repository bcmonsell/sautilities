#' Make a label for X-11-ARIMA M and Q statistics
#'
#' Generates a label for X-11-ARIMA M and Q statistics
#'
#' Version 1.2, 05/24/2023
#'
#' @param this_key character string; name of an X-11-ARIMA M and Q statistics used in the 
#'        UDG X-13 output. Default is \code{"f3.q"}.
#' @return character string with the corresponding label for \code{this_key}. 
#'         If incorrect label is specified, returns \code{NULL}.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' m7_label  <- get_mq_label('f3.m07')
#' @export
get_mq_label <- function(this_key = "f3.q") {
    # Author: Brian C. Monsell (OEUS) Version 1.2, 05/24/2023
    
    # Initialize \code{this_label}
    this_label <- NULL
    
    # Use \code{switch} to set \code{this_label} based on the value of \code{this_key}
    switch(tolower(this_key), f3.m01 = {
        this_label <- "M1"
    }, f3.m02 = {
        this_label <- "M2"
    }, f3.m03 = {
        this_label <- "M3"
    }, f3.m04 = {
        this_label <- "M4"
    }, f3.m05 = {
        this_label <- "M5"
    }, f3.m06 = {
        this_label <- "M6"
    }, f3.m07 = {
        this_label <- "M7"
    }, f3.m08 = {
        this_label <- "M8"
    }, f3.m09 = {
        this_label <- "M9"
    }, f3.m10 = {
        this_label <- "M10"
    }, f3.m11 = {
        this_label <- "M11"
    }, f3.q = {
        this_label <- "Q"
    }, f3.qm2 = {
        this_label <- "Q2"
    })
    
    
    return(this_label)
}
