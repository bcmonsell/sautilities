#' Sliding Spans Diagnostic 
#'
#' Results from using the sliding spans diagnostic.
#'
#' Version 3.8, 5/14/2024
#'
#' @param seas_obj object generated by \code{seas()} of the seasonal package.
#'        This is a required entry.
#' @param sf_limit Numeric object; limit for the percentage of seasonal spans flagged.
#'        Default is 25.
#' @param change_limit Numeric object; limit for the percentage of month-to-month changes flagged.
#'        Default is 40.
#' @param additivesa logical scalar; if true, the adjustment is assumed to be additive; 
#'        default is \code{FALSE}.
#' @param return_this character string; what the function returns - 
#'        \code{'test'} returns test results, \code{'why'} returns why the test failed or 
#'        received a warning, or \code{'both'}. Default is \code{'test'}.
#' @return A text string denoting if the series passed or failed the tests of sliding spans diagnostics.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' ukgas_seas <- seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
#'                              x11="", transform.function = "log", forecast.maxlead=20, 
#'                              slidingspans = "", check.print = c( "pacf", "pacfplot" ))
#' ukgas_sspan_test <- 
#'    sspan_test(ukgas_seas, sf_limit = 15, change_limit = 35, return_this = 'both')
#' @export
sspan_test <- function(seas_obj = NULL, sf_limit = 25, change_limit = 40, additivesa = FALSE, 
                       return_this = "test") {
    # Author: Brian C. Monsell (OEUS) Version 3.8, 5/14/2024
    
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
    
    
    # check to see if sliding spans diagnostics were estimated in this run.  
    # if not, return \code{'none'}
    this_udg <- seasonal::udg(seas_obj)
    if (this_udg[["sspans"]] == "no") {
        if (is_both) {
            return(cbind("fail","none"))

        } else {
            return("none")
        }
    }
    if (this_udg[["sspans"]] == "failed") {
        if (is_both) {
            return(cbind("fail","failed"))

        } else {
            return("failed")
        }
    }
    # check to see if differences were tested rather than percentages.  
    # If so, return \code{'diff'}
    if (this_udg[["ssdiff"]] == "yes") {
        if (is_both) {
            return(cbind("warn","diff"))

        } else {
            return("diff")
        }
    }
    # check to see if range is too small to generate precentages.  
    # If so, return \code{'no.pct'}
    this_test <- ""
    if (this_udg[["s2.pct"]] == "no") {
        if (is_both) {
            return(cbind("warn","no_pct"))
        } else {
            return("no_pct")
        }
    }

    if (additivesa) {
        # check to see if the percentage of seasonally adjusted series flagged is 
        # greater than \code{sf_limit}. If so, return \code{'warn'}
        is_this_add <- length(this_udg[["s2.c.per"]])
        if (is_this_add > 0) {
            this_key <- "s2.c.per"
        } else {
            this_key <- "s2.a.per"
        }
        if (this_udg[[this_key]][3] > sf_limit) {
            this_test <- "warn"
            this_why <- sspan_test_why(this_udg, sf_limit = sf_limit, change_limit = change_limit, 
                                       additivesa = additivesa, return_both = is_both)
        }
    } else {
        # check to see if the percentage of seasonal factors flagged is > \code{sf_limit}.  
        # If so, return \code{'warn'}.
        if (this_udg[["s2.a.per"]][3] > sf_limit) {
            this_test <- "warn"
            this_why <- 
                sspan_test_why(this_udg, sf_limit = sf_limit, change_limit = change_limit, 
                               additivesa = additivesa, return_both = is_both)
        }
    }
    # check to see if the percentage of changes flagged is > \code{change_limit}.  
    # If so, return \code{'warn'}
    if (this_udg[["s2.d.per"]][3] > change_limit) {
        this_test <- "warn"
        this_why <- 
            sspan_test_why(this_udg, sf_limit = sf_limit, change_limit = change_limit, 
                           additivesa = additivesa, return_both = is_both)
    }
    # else, return \code{'pass'}
    if (nchar(this_test) == 0) {
        this_test <- "pass"
        this_why <- "    "
        if (return_this == "why") {
            this_why <- "pass"
        }
    }
    
    # return \code{this_test} or \code{this_warn} based on value of \code{return_this}.
    
    if (is_both) {
        return(cbind(this_test, this_why))
    }
    if (is_test) {
        return(this_test)
    }
    return(this_why)
    
}
