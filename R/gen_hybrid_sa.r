#' Generate a hybrid seasonal adjustment
#'
#' Generates a "hybrid" seasonal adjustment by replacing a span of a multiplicative seasonal 
#' adjustment with an additive adjustment.
#'
#' Version 2.2, 5/25/2023
#'
#' @param this_mult_sa time series object of a multiplicative seasonal adjustment
#'        This is a required entry.
#' @param this_add_sa time series object of an additive seasonal adjustment
#'        This is a required entry.
#' @param this_start_hybrid integer vector of length 2, start of the span where additive  
#'        adjustments replace multiplicative adjustment. This is a required entry.
#' @param this_end_hybrid integer vector of length 2, end of the span where additive adjustments 
#'        replace multiplicative adjustment. This is a required entry.
#' @return time series object with hybrid seasonal adjustment.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' air_mult_seas <- seasonal::seas(AirPassengers, transform.function = "log")
#' air_mult_sa   <- seasonal::final(air_mult_seas)
#' air_add_seas  <- seasonal::seas(AirPassengers, transform.function = "none")
#' air_add_sa    <- seasonal::final(air_add_seas)
#' air_hybrid_sa <- gen_hybrid_sa(air_mult_sa, air_add_sa, c(1956,1), c(1956,12))
#' @export
gen_hybrid_sa <- function(this_mult_sa = NULL, this_add_sa = NULL, this_start_hybrid = NULL, 
                          this_end_hybrid = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 2.2, 5/25/2023
    # check if a value is specified for \code{this_mult_sa}
    if (is.null(this_mult_sa)) {
        stop("must specify a multiplicative adjustment")
    }
    
    # check if a value is specified for \code{this_add_sa}
    if (is.null(this_add_sa)) {
        stop("must specify an additive adjustment")
    } 
    
    # check if a value is specified for \code{this_start_hybrid}
    if (is.null(this_start_hybrid)) {
        stop("must specify starting date for hybrid adjustment")
    }
    
    # check if a value is specified for \code{this_end_hybrid}
    if (is.null(this_end_hybrid)) {
        stop("must specify ending date for hybrid adjustment")
    } 
    
    this_start <- start(this_mult_sa)
    this_end   <- end(this_mult_sa)
    this_freq  <- frequency(this_mult_sa)
    
    before_hybrid_start <- this_start_hybrid - c(0,1)
    if (before_hybrid_start[2] == 0) {
        before_hybrid_start <- before_hybrid_start + c(-1, this_freq)
    }
    
    after_hybrid_end <- this_end_hybrid + c(0,1)
    if (after_hybrid_end[2] > this_freq){
        after_hybrid_end <- after_hybrid_end + c(1, -this_freq)
    }
    
    this_hybrid_sa <- 
       ts(c(window(this_mult_sa, end = before_hybrid_start),
         window(this_add_sa, start = this_start_hybrid, end = this_end_hybrid),
         window(this_mult_sa, start = after_hybrid_end)), start = this_start,
         frequency = this_freq)
         
    return(this_hybrid_sa)
    
}