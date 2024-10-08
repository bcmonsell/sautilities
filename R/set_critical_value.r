#' Set outlier critical value
#'
#' Set outlier critical value using the Ljung algorithm as given in
#' Ljung, G. M. (1993). On outlier detection in time series. 
#' Journal of Royal Statistical Society B 55, 559-567.
#' 
#' Version 1.6 5/25/2023
#'
#' @param number_observations number of observations tested for outliers  
#'        This is a required entry.
#' @param cv_alpha alpha for critical value
#' @return outlier critical value generated by the algorithm given in Ljung (1993). 
#'         The critical value in X-13 is different as it is adjusted to allow for smaller values 
#'         to approximate the normal distribution.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' this_critical_value <- set_critical_value(12, 0.025)
#' @export
set_critical_value <- function(number_observations = NULL, cv_alpha = 0.01) {
    # Brian Monsell Version 1.6 5/25/2023
    
    # check if a value is specified for \code{number_observations}
    if (is.null(number_observations)) {
        stop("must specify number of observations.")
    }

    pmod <- 2 - sqrt(1 + cv_alpha)
    acv <- sqrt(2 * log(number_observations))
    bcv <- acv - (log(log(number_observations)) + log(2 * 2 * pi))/(2 * acv)
    xcv <- -log(-0.5 * log(pmod))
    setcvl <- (xcv/acv) + bcv
    return(setcvl)
}
