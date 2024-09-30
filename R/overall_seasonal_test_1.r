#' First overall sasonality test from Maravall (2012)
#'
#' Conduct the first overall test for seasonality as laid out in Maravall (2012)
#'
#' Version 3.4, 5/21/2024
#'
#' @param seas_obj \code{seas} object for a single series
#'        This argument is required.
#' @param this_series character string; the table used to generate the AR(30) spectrum. 
#'        Default is \code{"a1"}.
#' @param take_log logical scalar; indicates if the AR spectrum is generated from the log 
#'        of the data. Default is \code{TRUE}.
#' @param this_ori_qs character string; code for which original series QS will be used in this 
#'        analysis. Default is \code{"qsori"} (original series, full span); other choices are  
#'        \code{"qssorievadj"} (original series adjusted for extreme values,short span), 
#'        \code{"qsorievadj"} (original series adjusted for extreme values, full span), and 
#'        \code{"qssori"} (original series, short span)
#' @return A list with 3 elements: \code{QStest} (test probability for QS), 
#'         \code{NPtest} (test probability for NP),
#'         and \code{result} (character string with test result - possible values of either
#'         \code{"evidence of seasonality"} and \code{"no evidence of seasonality"})
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' air_seas <- seasonal::seas(AirPassengers,
#'                     arima.model="(0 1 1)(0 1 1)",
#'                     forecast.maxlead = 36, slidingspans = "",
#'                     transform.function = "log", 
#'                     series.save = "a1")
#' first_test <- overall_seasonal_test_1(air_seas)
#' @export
overall_seasonal_test_1 <- 
    function(seas_obj = NULL, this_series = "a1", take_log = TRUE, 
	         this_ori_qs = "qsori") {
    # Author: Brian C. Monsell (OEUS) Version 3.4, 5/21/2024

    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
           stop("First argument must be a seas object")
        }
    }

    this_x <- seasonal::series(seas_obj, this_series)
    if (take_log) { this_x <- log(this_x) }

    this_NP     <- NP_test(this_x)
    this_QS     <- 1.0 - as.vector(seasonal::udg(seas_obj, this_ori_qs))[2]

    if (this_QS > 0.99) {
        this_result <- "evidence of seasonality"
    } else {
        if (this_QS > 0.95 && this_NP$cv > 0.95) {
            this_result <- "evidence of seasonality"
        } else {
            this_result <- "no evidence of seasonality"
        }
    }

    return(list(QStest = this_QS, NPtest = this_NP$cv, result = this_result))
}
