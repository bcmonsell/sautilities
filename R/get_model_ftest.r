#' Get model based F-test
#'
#' Extract values associated with the model based F-test specified by the \code{this_ftest} argument.
#'
#' Version 3.8, 5/15/2024
#'
#' @param seas_obj A seas object for a single series generated from the \code{seasonal} package
#'        This is a required entry.
#' @param this_ftest Character string; type of model based f-test to return.  
#'        Default is \code{"seasonal"}; only other acceptable value is \code{"td"}.
#' @param return_this Character string, Code that controls what values are returned. 
#'        Acceptable values are \code{"all"}, \code{"dof"} (degrees of freedom), \code{"ftest"},
#'        (F-test value), or \code{"pval"} (F-test p-value). Default is \code{"all"}.
#' @return Numeric vector with seasonal or trading day F-statistic, degrees of freedom, p-value. 
#'         If not found, return \code{NULL}.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' air_seas_td_seasonal <- 
#'     seasonal::seas(AirPassengers, arima.model = "(0 1 1)", 
#'                    regression.variables = c("seasonal", "td"), x11="")
#' this_seas_ftest <- get_model_ftest(air_seas_td_seasonal, this_ftest = "seasonal", 
#'                                    return_this = "ftest")
#' this_td_ftest   <- get_model_ftest(air_seas_td_seasonal, this_ftest = "td", 
#'                                    return_this = "ftest")
#' @export
get_model_ftest <- function(seas_obj = NULL, this_ftest = "seasonal", return_this = "all") {
    # Author: Brian C. Monsell (OEUS) Version 3.8, 5/15/2024
    
    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
           stop("First argument must be a seas object")
        }
    }
    
    # generate key for extracting f-test
    this_ftest <- tolower(this_ftest)
    if (this_ftest == "seasonal" | this_ftest == "td") {
        if (this_ftest == "td") {
            this_key <- "ftest$Trading Day"
        } else {
            this_key <- "ftest$Seasonal"
        }
    } else {
        print(paste("this keyword not found: ", this_ftest, sep = ""))
        return(NULL)
    }
    
    # save all UDG entries
    this_udg <- seasonal::udg(seas_obj)
    
    # get position of the seasonal f-test; if 0, return NULL
    this_udg_index <- get_udg_index(this_udg, this_key)
    if (this_udg_index == 0) {
        return(NULL)
    }
    
    this_ftest_vector <- this_udg[[this_udg_index]]
    switch(tolower(return_this),
        "all" = { this_return <- this_ftest_vector },
        "dof" = { this_return <- this_ftest_vector[c(1,2)] },
        "ftest" = { this_return <- this_ftest_vector[3] },
        "pval" = { this_return <- this_ftest_vector[4] })
    
    return(this_return)
    
}
