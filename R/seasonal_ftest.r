#' Model-based F-Test for Time Series Models.
#'
#' Model based test for seasonality based on stable seasonal regressors
#'
#' Version 5.2, 5/14/2024
#'
#' @param seas_obj object generated by \code{seas()} of the seasonal package.
#'        This is a required entry.
#' @param p_limit_fail Numeric scalar; P-value limit for model based seasonal F-statistic 
#'        for passing. Default is \code{0.01}.
#' @param p_limit_warn Numeric scalar; P-value limit for model based seasonal F-statistic 
#'        for a warning. Default is \code{0.05}.
#' @param use_seasonal logical scalar; if TRUE, the \code{seasonal} regressor is used;
#'        otherwise, use the sine-cosine trignonmetric regressors generated by \code{sincos}.
#' @param return_this character string; what the function returns - 
#'        \code{'test'} returns test results, \code{'why'} returns why the test failed or 
#'        received a warning, or \code{'both'}. Default is \code{'test'}.
#' @return A text string denoting if the series passed or failed tests for seasonality 
#'         using the model based F-test diagnostic.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' m_air <- 
#'   seasonal::seas(AirPassengers, transform.function = "log", arima.model = '(0 1 1)', 
#'                  regression.variables = c('seasonal', 'td'), x11="")
#' this_seasonal_ftest <- seasonal_ftest(m_air, return_this = 'both')
#' @export
seasonal_ftest <- function(seas_obj = NULL, p_limit_fail = 0.01, p_limit_warn = 0.05, 
                           use_seasonal = TRUE, return_this = "test") {
    # Author: Brian C. Monsell (OEUS) Version 5.2, 5/14/2024
    
    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
           stop("First argument must be a seas object")
        }
    }
    
    # set \code{is_test}, \code{is_why}, \code{is_both}
    is_test <- FALSE
    is_why <- FALSE
    is_both <- FALSE
    return_this <- tolower(return_this)
    if (return_this == "test") {
        is_test <- TRUE
    }
    if (return_this == "why") {
        is_why <- TRUE
    }
    if (return_this == "both") {
        is_test <- TRUE
        is_why <- TRUE
        is_both <- TRUE
    }
    
    # check for improper entry for \code{return_this}
    if (!is_test & !is_why) {
        print(paste("Improper entry for return_this : ", return_this, sep = ""))
        return(NULL)
    }
    
	# set key for seasonal F-test
	if (use_seasonal) {
	    this_key <- "ftest$Seasonal"
	} else {
	    this_key <- "ftest$Trigonometric Seasonal"
	}

    # save all UDG entries
    udg_list <- seasonal::udg(seas_obj)
	
    # get position of the seasonal f-test; if 0, return message that f-test not generated
    index.f.test <- get_udg_index(udg_list, this_key)
    if (index.f.test == 0) {
        if (is_both) {
            return(c("no_f_stats","    "))
        } else {
            return("no_f_stats")
        }
    }
    
    # test if p-value is greater than the warn limit; if so, this passes
    if (udg_list[[this_key]][4] <= p_limit_fail) {
        this_test <- "pass"
        this_why <- "    "
        if (return_this == "why") {
            this_why <- "pass"
        }
    } else {
        # else test if p-value is less than the fail limit; if so, warning
        if (udg_list[[this_key]][4] <= p_limit_warn) {
            this_test <- "warn"
            this_why <- paste(p_limit_fail, "< plimit <= ", p_limit_warn, sep = "")
            if (!is_both) {
                this_why <- paste("warn:", this_test, sep = " ")
            }
        } else {
            this_test <- "fail"
            this_why <- paste("plimit > ", p_limit_warn, sep = "")
            if (!is_both) {
                this_why <- paste("fail:", this_test, sep = " ")
            }
        }
    }
    
    # return \code{this_test} or \code{this_warn} based on value of \code{return_this}
    
    if (is_both) {
        return(cbind(this_test, this_why))
    }
    if (is_test) {
        return(this_test)
    }
    return(this_why)
    
}
