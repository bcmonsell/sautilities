#' Optimal X-11 seasonal moving average selection
#'
#' Determine the optimal X-11 seasonal moving average based on the value of the seasonal moving 
#' average coefficient from an airline model.
#'
#' Version 4.3, 5/25/2023
#'
#' @param this_series A time series object. This is a required entry.
#' @param aictest a character string with the entries for the \code{regression.aictest} argument 
#'        to the \code{seas} function from the \code{seasonal} package. 
#'        Default is \code{NULL}, AIC testing not done.
#' @param model a character string with the entry for the \code{arima.model} argument to the 
#'        \code{seas} function from the \code{seasonal} package. 
#'        Default is \code{"(0 1 1)(0 1 1)"}. Model should have a \code{(0 1 1)} seasonal term.
#' @param variables a character string with the entries for the \code{regression.variables} 
#'        argument to the \code{seas} function from the \code{seasonal} package. 
#'        Default is \code{NULL}, no regressors added.
#' @param outlier logical scalar, if \code{TRUE} outlier identification is done in the call  
#'        to the \code{seas} function from the \code{seasonal} package. 
#'        Default is \code{TRUE}.
#' @param trans characater scalar, a character string with the entry for the 
#'        \code{transform.function} argument to the \code{seas} function.
#'        Default is \code{NULL}, and the entry \code{auto} will be used.
#' @param missing_code numeric scalar, a number with the entry for the \code{series.missingcode} 
#'        argument to the \code{seas} function. 
#'        Default is \code{NULL}, no missing value code is used.
#' @param this_xreg numeric matrix, a user defined regressor matrix to be used in the model 
#'        estimation. Default is \code{NULL}, no user-defined regressors are used.
#' @param dp_limits logical scalar, if \code{TRUE} limits from Deputot and Planas will be used  
#'        to choose the moving average, else limits from Bell Chow and Chu will be used. 
#'        Default is \code{TRUE}.
#' @param use_msr logical scalar, if \code{TRUE} result of MSR selection will be used if model cannot 
#'        be estimated, otherwise function will return a \code{NULL} value. 
#'        Default is \code{FALSE}.
#' @param use_3x15 logical scalar, if \code{TRUE} 3x15 seasonal filter will be returned if chosen, 
#'        otherwise function will return a \code{3x9} value. 
#'        Default is \code{TRUE}.
#' @return The optimal X-11 seasonal filter, unless the airline model cannot be estimated.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' this_seasonal  <- 
#'    optimal_seasonal_filter(shoes2008, aictest = c("td", "easter"), use_msr = TRUE)
#' this_seasonal2 <- 
#'    optimal_seasonal_filter(shoes2008, aictest = c("td", "easter"), dp_limits = FALSE, 
#'                            use_msr = TRUE)
#' @import stats
#' @export
optimal_seasonal_filter <- function(this_series = NULL, aictest = NULL, model = "(0 1 1)(0 1 1)", 
    variables = NULL, outlier = TRUE, trans = NULL, missing_code = NULL, this_xreg = NULL, 
    dp_limits = TRUE, use_msr = FALSE, use_3x15 = TRUE) {
    # Author: Brian C. Monsell (OEUS) Version 4.3, 5/25/2023
    
    # check if a value is specified for \code{this_series}
    if (is.null(this_series)) {
        stop("must specify a time series object")
    }
    
    # set option for transformation
    if (is.null(trans)) {
        trans_text <- "auto"
    } else {
        trans_text <- trans
    }
    
    # generate \code{seas} object for the series with an airline model, 
    # user supplied transformation, regressors, and choice of transformation
    if (outlier) {
        this_seas_object <- seasonal::seas(this_series, transform.function = trans_text, 
                x11 = "", arima.model = model, slidingspans = NULL, xreg = this_xreg, 
                regression.variables = variables, regression.aictest = aictest, 
                series.missingcode = missing_code)
    } else {
        this_seas_object <- seasonal::seas(this_series, transform.function = trans_text, 
                x11 = "", arima.model = model, xreg = this_xreg, slidingspans = NULL, 
                regression.variables = variables, regression.aictest = aictest, 
                series.missingcode = missing_code, outlier = NULL)
    }
    
    # extract seasonal theta
    this_seasonal_theta <- 
        get_seasonal_theta(this_seas_object, frequency(this_series), return_string = FALSE)
    
    # choose seasonal filter. If filter chosen, return this value
    this_seasonal_filter <- 
        choose_optimal_seasonal_filter(this_seasonal_theta, dp_limits, use_3x15)
        
    if (!is.null(this_seasonal_filter)) {
        return(this_seasonal_filter)
    }
    
    # if \code{use_msr} is true, extract MSR choice of seasonal factor if UDG keyword not found, 
    # print out error message and set \code{this_seasonal} to \code{NULL}
    if (use_msr) {
        this_seasonal_filter <- 
            tryCatch(seasonal::udg(this_seas_object, "sfmsr"), error = function(e) {
                print(paste("this keyword not found: sfmsr", sep = ""))
                NULL
        })
        return(this_seasonal_filter)
    }
    
    return(NULL)
    
}
