#' Test for spectral peaks
#'
#' Test if spectral peaks are flagged.
#'
#' Version 4.0, 05/3/2024
#'
#' @param seas_obj object generated by \code{seas()} of the \code{seasonal} package.
#'        This is a required entry.
#' @param peak_level Integer scalar - limit to determine if a frequency has a spectral peak.
#'        Default is 6.
#' @param peak_warn Integer scalar - limit to produce a warning that a frequency may have a 
#'        spectral peak. Default is 3.
#' @param this_spec text string with the spectrum being tested. Allowable entries are 
#'        \code{'spcori'}, \code{'spcsa'}, \code{'spcirr'}, \code{'spcrsd'}. 
#'        Default is \code{'spcori'}.
#' @param return_this character string; what the function returns - 
#'        \code{'test'} returns test results, \code{'why'} returns why the test failed or 
#'        received a warning, or \code{'both'}. Default is \code{'test'}.
#' @return A text string denoting if the series passed or failed the tests of spectrum 
#'         diagnostics. Note that for \code{spcori}, the series fails if none of the 
#'         frequencies tested had peaks.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' air_seas <- seasonal::seas(AirPassengers, transform.function = "log", 
#'                         arima.model = "(0 1 1)(0 1 1)", x11 = "")
#' this_spec_peak_test <- spec_peak_test(air_seas, this_spec = 'spcori', return_this = 'both')
#' @export
spec_peak_test <- function(seas_obj = NULL, peak_level = 6, peak_warn = 3, this_spec = "spcsa", 
                           return_this = "test") {
    # Author: Brian C. Monsell (OEUS) Version 4.0, 05/3/2024
    
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
    
    this_spec <- tolower(this_spec)
    udg_list <- seasonal::udg(seas_obj)
    if (udg_list[["spectrum"]] == "no") {
        if (is_both) {
            return(c("none","    "))
        } else {
            return("none")
        }
    }
    # check to see if the value of spec is valid.  if not, print error message
    good_spec <- c("spcori", "spcsa", "spcirr", "spcrsd")
    is_arg <- grep(this_spec, good_spec)
    if (length(is_arg) == 0) {
        stop(paste("Value of thisStat '", this_spec, "' not allowed.", sep = ""))
    }
    # initialize number of peaks to zero
    n_peaks <- 0
    # check to see if one of the seasonal frequencies has a peak > peak_level.  if so, 
    # update number of peaks
    for (i in 1:5) {
        this_key <- paste(this_spec, ".s", i, sep = "")
        this_peak <- udg_list[[this_key]]
        this_length <- length(this_peak)
        if (this_length > 1) {
            if (as.numeric(this_peak[1]) > peak_level) {
                n_peaks <- n_peaks + 1
            }
        }
    }
    
    if (this_spec == "spcori") {
        n_seasonal_peak <- n_peaks
    }
    
    # check to see if the first trading day frequency has a peak > peak_level.  if so, 
    # update number of peaks
    this_key <- paste(this_spec, ".t1", sep = "")
    this_peak <- udg_list[[this_key]]
    this_length <- length(this_peak)
    if (this_length > 1) {
        if (as.numeric(this_peak[1]) > peak_level) {
            n_peaks <- n_peaks + 1
        }
    }
    is_fail <- FALSE
    if (this_spec == "spcori") {
        # if the number of peaks found is = 0, return 'fail'
        if (n_seasonal_peak < 1) {
            is_fail <- TRUE
        }
        if (n_peaks > n_seasonal_peak) {
            if (n_seasonal_peak < 1) {
                is_fail <- TRUE
            }
        }
    } else {
        # if the number of peaks found is > 0, return 'fail'
        if (n_peaks > 0) {
            is_fail <- TRUE
        }
    }
    # reset number of peaks to zero
    n_peaks <- 0
    is_warn <- FALSE
    if (!is_fail) {
        # check to see if one of the seasonal frequencies has a peak > \code{peak_warn}.  
        # if so, update number of 'warning' peaks
        if (this_spec == "spcsa" | this_spec == "spcirr" | this_spec == "spcrsd") {
            for (i in 1:5) {
                this_key <- paste(this_spec, ".s", i, sep = "")
                this_peak <- udg_list[[this_key]]
                this_length <- length(this_peak)
                if (this_length > 1) {
                    if (this_peak[1] != "nopeak") {
                        if (as.numeric(this_peak[1]) > peak_warn) {
                            n_peaks <- n_peaks + 1
                        }
                    }
                }
            }
        }
        # check to see if the second trading day frequency has a peak > \code{peak_level}.  
        # if so, update number of 'warning' peaks
        this_key <- paste(this_spec, ".t2", sep = "")
        this_peak <- udg_list[[this_key]]
        this_length <- length(this_peak)
        if (this_length > 1) {
            if (as.numeric(this_peak[1]) > peak_level) {
                n_peaks <- n_peaks + 1
            }
        }
        # if the number of 'warning' peaks found is > 0, return 'warn'
        if (n_peaks > 0) {
            is_warn <- TRUE
        }
    }
    
    this_why <- "    "
    if (is_fail) {
        this_test <- "fail"
        this_why <- 
           spec_peak_fail_why(udg_list, peak_level = peak_level, this_spec = this_spec, 
                              return_both = is_both)
    } else {
        if (is_warn) {
            this_test <- "warn"
            this_why <- 
                spec_peak_warn_why(udg_list, peak_warn_level = peak_warn, this_spec = this_spec, 
                                   return_both = is_both)
        } else {this_why
            this_test <- "pass"
            if (return_this == "why") {
                this_why <- "pass"
            }
        }
    }
    
    # return \code{this_test} or \code{this_why} based on value of \code{return_this}
    
    if (is_both) {
        return(cbind(this_test, this_why))
    }
    if (is_test) {
        return(this_test)
    }
    return(this_why)
    
}

