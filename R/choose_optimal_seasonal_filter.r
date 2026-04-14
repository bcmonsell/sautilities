#' Choose Optimal X-11 seasonal moving average 
#'
#' Choose the optimal X-11 seasonal moving average based on the value of the seasonal moving 
#' average coefficient from an airline model.
#'
#' Version 2.0, 2026-02-24
#'
#' @param this_seasonal_theta numeric scalar; seasonal moving average coefficient from an 
#'        airline model. This is a required entry.
#' @param dp_limits logical scalar, if \code{TRUE} limits from 
#'        \insertCite{PlanasDepoutot2002;textual}{sautilities} will be used  
#'        to choose the moving average, else limits from 
#'        \insertCite{ChuTiaoBell2012;textual}{sautilities} will be used. 
#'        Default is \code{TRUE}.
#' @param use_3x15 logical scalar, if \code{TRUE} 3x15 seasonal filter will be returned if chosen, 
#'        otherwise function will return a \code{3x9} value. 
#'        Default is \code{FALSE}.
#' @return The optimal X-11 seasonal filter.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' shoes_seas <- 
#'    seasonal::seas(shoes2008, 
#'                   x11="", slidingspans = "", 
#'                   transform.function = "log", 
#'                   arima.model = "(0 1 1)(0 1 1)", 
#'                   regression.aictest = c("td", "easter"),
#'                   forecast.maxlead=36, 
#'                   check.print = c( "pacf", "pacfplot" ))
#' shoes_seasonal_MA <- shoes_seas$est$coefficients[["MA-Seasonal-12"]]
#' this_seasonal  <- 
#'     choose_optimal_seasonal_filter(shoes_seasonal_MA)
#' this_seasonal2 <- 
#'     choose_optimal_seasonal_filter(shoes_seasonal_MA, dp_limits = FALSE)
#' @importFrom Rdpack reprompt
#' @export
choose_optimal_seasonal_filter <- 
   function(this_seasonal_theta = NULL, 
            dp_limits = TRUE, 
            use_3x15 = TRUE) {
    # Author: Brian C. Monsell (OEUS) Version 2.0, 2026-02-24

    if (is.null(this_seasonal_theta)) {
        return(NULL)
    }    
    if (dp_limits) {
        if (this_seasonal_theta > 0.87) {
            if (use_3x15) {
              return("3x15")
            } else {
              warning("changing 3x15 to 3x9")
              return("3x9")
            }
        }
        if (this_seasonal_theta > 0.74) {
            return("3x9")
        }
        if (this_seasonal_theta > 0.5) {
            return("3x5")
        }
        return("3x3")
    } else {
        if (this_seasonal_theta > 0.75) {
            if (use_3x15) {
              return("3x15")
            } else {
              warning("changing 3x15 to 3x9")
              return("3x9")
            }
        }
        if (this_seasonal_theta > 0.65) {
            return("3x9")
        }
        if (this_seasonal_theta > 0.45) {
            return("3x5")
        }
        if (this_seasonal_theta > 0.25) {
            return("3x3")
        }
        return("3x1")
    }
}
