#' Subspan time series
#'
#' Generate subspan of time series
#'
#' Version 2.3, 5/25/2023
#'
#' @param X Time Series object.
#'        This is a required entry.
#' @param plot_start Integer vector of length 2; Starting date for plot. 
#'        Default is starting date for the time series. 
#' @param plot_end Integer vector of length 2; Starting date for plot. 
#'        Default is ending date for the time series. 
#' @return generate subspan of time series \code{X} specified by \code{plot_start} and 
#'         \code{plot_end}.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' air50 <- get_window(AirPassengers, plot_start = c(1950,1), plot_end = c(1959,12))
#' @import stats
#' @export
get_window <- function(X = NULL, plot_start = NULL, plot_end = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 2.3, 5/25/2023
    
    # check if a value is specified for \code{X}
    if (is.null(X)) {
        stop("must specify a time series")
    }

    if (is.null(plot_start)) {
        if (is.null(plot_end)) {
            # if \code{plot_start} and \code{plot_end} are NULL, return \code{X}
            return(X)
        } else {
            # if only \code{plot_start} is NULL, generate truncated \code{X}
            X <- window(X, end = plot_end)
        }
    } else {
        if (is.null(plot_end)) {
            11
            # if only \code{plot_end} is NULL, generate \code{X} with new starting date
            X <- window(X, start = plot_start)
        } else {
            # if neither is NULL, generate subspan of \code{X} defined by \code{plot_start} 
            # and \code{plot_end}
            X <- window(X, start = plot_start, end = plot_end)
        }
    }
    # Return \code{X}
    return(X)
}
