#' Generate the either the average or median absolute percentage difference between two vectors
#'
#' Generate the average absolute percentage difference (AAPD) or median absolute percentage difference (MAPD) 
#' between two vectors - the vectors must be the same length.
#'
#' Version 2.0, 6/21/2024
#'
#' @param x1 numeric vector; This is a required entry.
#' @param x2 numeric vector; This is a required entry, and must be the same length as \code{x1}.
#' @param use_median logical scalar, if \code{TRUE}, returns the median of the absolute percentage difference;
#'        if \code{FALSE}, returns the average of the absolute percentage difference
#'        Default is \code{FALSE}.
#' @return Either the average or median absolute percentage difference of the two series.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' ukgas_seas <- 
#'    seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
#'                   x11="", transform.function = "log", forecast.maxlead=20,
#'                   check.print = c( "pacf", "pacfplot" ))
#' ukgas_seats_seas <- 
#'    seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
#'                   transform.function = "log",, forecast.maxlead=20,
#'                   check.print = c( "pacf", "pacfplot" ))
#' ukgas_x11_sa     <- seasonal::final(ukgas_seas)
#' ukgas_seats_sa   <- seasonal::final(ukgas_seats_seas)
#' ukgas_sa_aapd    <- 
#'    sautilities::absolute_pct_diff(ukgas_seats_sa, ukgas_x11_sa)
#' ukgas_sa_mapd    <- 
#'    sautilities::absolute_pct_diff(ukgas_seats_sa, ukgas_x11_sa, use_median = TRUE)
#' @export
absolute_pct_diff <- function(x1 = NULL, x2 = NULL, use_median = FALSE) {
    # Author: Brian C. Monsell (OEUS) Version 2.0, 6/21/2024

   if (is.null(x1)) {
       return(NULL)
   }
   if (is.null(x2)) {
       return(NULL)
   }
   
   # generate length of x1, check if x2 is same length
   n1 <- length(x1)
   if (n1 != length(x2)) {
       stop("the two series need to be the same length")
   }
   
   # generate and return AAPD 
   abs_pct_diff <- (abs(x1 - x2) / x2) * 100.0
   if (use_median) {
       return(median(abs_pct_diff))
   } else {
       return(sum(abs_pct_diff)/n1)
   }
}