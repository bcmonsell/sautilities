#' Import File Saved by X-13ARIMA-SEATS 
#'
#' Import data from a file saved by the X-13ARIMA-SEATS program
#'
#' Version 1.2, 5/25/2023
#'
#' @param filename Character string, filename of a file saved by the \code{X-13ARIMA-SEATS}  
#'        program. This is a required entry.
#' @param pos Integer scalar, column of data to be extracted from \code{filename}. 
#'        Default is 2.
#' @param ncol Integer scalar, number of columns of data that exist within \code{filename}. 
#'        Default is 2.
#' @return A time series array
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' \dontrun{
#' airline.sa <- input_saved_x13_file("airline.d11")
#' }
#' @import stats
#' @export
input_saved_x13_file <- function(filename = NULL, pos=2, ncol=2) {
    # Author: Brian C. Monsell (OEUS), Version 1.2, 5/25/2023
   if(is.null(filename)) stop("must specify filename")
   wlist <-vector("list", ncol)
   for (i in 1:ncol) wlist[[i]] <-0
   temp <-scan(filename, what=wlist, skip=2)
   f <-max(temp[[1]]%%100)
   obs1 <-c(temp[[1]][1]%/%100, temp[[1]][1]%%100)
   ts(temp[[pos]], start=obs1, frequency=f)
}