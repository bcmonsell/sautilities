#' Model diagnostic summary 
#'
#' Generate a summary of model diagnostics for a single series 
#'
#' Version 5.5, 6/21/2024
#'
#' @param seas_obj \code{seas} object generated from a call of \code{seas} on a single time series 
#'        This is a required entry.
#' @param add_aicc logical scalar; add AICC value to the summary.
#'        Default is \code{FALSE}
#' @param add_norm logical scalar; add normality statistics to the summary.
#'        Default is \code{FALSE}
#' @param add_auto_out logical scalar; add identified automatic outliers to the summary.
#'        Default is \code{FALSE}
#' @param add_seasonal logical scalar; add QS test for seasonality to the summary. 
#'        Default is \code{FALSE}.
#' @param add_spec logical scalar; add test for spectral peaks to the summary. 
#'        Default is \code{FALSE}.
#' @param add_lbq logical scalar; add test for Ljung-Box Q to the summary.
#'        Default is \code{TRUE}; if set to \code{FALSE}, a more extensive test of ACF
#'        will be done.
#' @param set_lbq_lags_fail - lags of the Ljung-Box Q to test
#'        Default is \code{c(12, 24)}.
#' @param set_lbq_p_limit Numeric scalar; P-value limit for Ljung-Box Q test
#'        Default is \code{0.01}.
#' @param set_acf_num_sig Limit for number of lags with significant ACF values
#'        Default is \code{8}.
#' @param set_acf_lags_fail - lags of the ACF to test
#'        Default is \code{c(1, 2, 3, 4, 12, 24)}.
#' @param set_acf_lags_warn - lags of the ACF to test for warnings
#'        Default is \code{c(12, 24)}.
#' @param add_pacf logical scalar; include PACF in ACF results. 
#'        Default is \code{TRUE}.
#' @param qs_test_span logical scalar; test span of data in QS seasonal test rather than full series. 
#'        Default is \code{FALSE}
#' @param set_qs_p_limit_pass Numeric scalar; P-value limit for QS statistic for passing
#'        Default is \code{0.01}.
#' @param set_qs_p_limit_warn Numeric scalar; P-value limit for QS statistic for warning
#'        Default is \code{0.05}.
#' @param set_qs_robust_sa Logical scalar indicating if original series adjusted for extremes is 
#'        included in testing. Default is \code{TRUE}.
#' @param set_spec_peak_level Integer scalar - limit to determine if a frequency has a spectral peak.
#'        Default is \code{6}.
#' @param set_spec_peak_warn Integer scalar - limit to produce a warning that a frequency may have a 
#'        spectral peak. Default is \code{3}.
#' @param return_list logical scalar; return a list rather than a vector.
#'        Default is \code{FALSE}
#' @return vector or list of model diagnostics for a given series
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' air_seas <- 
#'    seasonal::seas(AirPassengers, x11="", slidingspans = "", 
#'                   transform.function = "log", arima.model = "(0 1 1)(0 1 1)", 
#'                   regression.aictest = "td", forecast.maxlead=36, 
#'                   check.print = c( "pacf", "pacfplot" ), check.save = "acf")
#' air_diag <- 
#'     all_model_diag(air_seas, add_seasonal = TRUE, add_aicc = TRUE, add_norm = TRUE, 
#'                    add_auto_out = TRUE, add_pacf = FALSE, qs_test_span = TRUE, 
#'                    return_list = TRUE)
#' @export
all_model_diag <- function(seas_obj = NULL, add_aicc = FALSE, add_norm = FALSE,  
   add_auto_out = FALSE, add_seasonal = FALSE, add_spec = FALSE, add_lbq = TRUE, 
   set_lbq_lags_fail = c(12, 24), set_lbq_p_limit = 0.01, set_acf_num_sig = 8, 
   set_acf_lags_fail = c(1, 2, 3, 4, 12, 24), set_acf_lags_warn = c(12, 24), 
   add_pacf = TRUE, qs_test_span = FALSE, set_qs_p_limit_pass = 0.01, 
   set_qs_p_limit_warn = 0.05, set_qs_robust_sa = TRUE, 
   set_spec_peak_level = 6, set_spec_peak_warn = 3, return_list = FALSE) {
    # Author: Brian C. Monsell (OEUS) Version 5.5, 6/21/2024
    
    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
            stop("First argument must be a seas object")
        }
    }
    
    # get ARIMA Model
    this_model <- seasonal::udg(seas_obj, "arimamdl")
    
    # get number of regressors
    this_nreg <- seasonal::udg(seas_obj, "nreg")
    
    # add automatic outliers, if specified
    if (add_auto_out) {
        this_autoout <- get_udg_entry(seas_obj, "autoout")
        this_autoreg <- " "
        if (is.null(this_autoout)) {
            this_autoout <- 0
        } else {
            this_autoreg <- get_auto_outlier_string(seas_obj)
        }
    }
    
    # generate residual ACF test result
    if (add_lbq) {
        this_lbq_test <-
            lbq_test(seas_obj, lbq_lags_fail = set_lbq_lags_fail, 
                     p_limit = set_lbq_p_limit, return_this = "why")
        # create summary
        this_summary <- 
            c(this_model, this_nreg, this_lbq_test)

        # initialize a vector of names
        this_names <- 
            c("ARIMA_Model", "Number_of_Reg", "LBQ_Test")
    } else {
        this_acf_test <- 
            acf_test(seas_obj, num_sig = set_acf_num_sig, acf_lags_fail = set_acf_lags_fail, 
                     acf_lags_warn = set_acf_lags_warn, include_pacf = add_pacf, 
                     return_this = "why")
        # create summary
        this_summary <- 
            c(this_model, this_nreg, this_acf_test)

        # initialize a vector of names
        this_names <- 
            c("ARIMA_Model", "Number_of_Reg", "ACF_Test")
    }
    
    # generate seasonality test using QS
    if (add_seasonal) {
        if (qs_test_span) {
            this_seasonal_test <- 
                 qs_seasonal_test(seas_obj, test_full = TRUE, test_span = FALSE, 
                                  p_limit_pass = set_qs_p_limit_pass, 
                                  p_limit_warn = set_qs_p_limit_warn, 
                                  robust_sa = set_qs_robust_sa, return_this = "why")
        } else {
            this_seasonal_test <- 
                 qs_seasonal_test(seas_obj, test_full = FALSE, test_span = TRUE, 
                                  p_limit_pass = set_qs_p_limit_pass, 
                                  p_limit_warn = set_qs_p_limit_warn, 
                                  robust_sa = set_qs_robust_sa, return_this = "why")
        }
        this_summary <- c(this_summary, this_seasonal_test)
    }
    
    # generate seasonality test using spectral peaks
    if (add_spec) {
        this_spec_peak_test <- 
            spec_peak_test(seas_obj, peak_level = set_spec_peak_level, peak_warn = set_spec_peak_warn, 
                           this_spec = "spcori", return_this = "why")
        this_summary <- c(this_summary, this_spec_peak_test)
    }
    

    # add aicc, if specified
    if (add_aicc) {
        this_aicc <- seasonal::udg(seas_obj, "aicc")
        this_summary <- c(this_summary, this_aicc)
    }
    
    # add normality statistics, if specified
    if (add_norm) {
        this_a <- get_norm_stat(seas_obj, "a")
        if (!is.null(this_a)) {
            this_summary <- 
                c(this_summary, replace_na(this_a), replace_na(norm_test(seas_obj, "a")))
        }
        this_kurtosis <- get_norm_stat(seas_obj, "kurtosis")
        if (!is.null(this_kurtosis)) {
            this_summary <- 
                c(this_summary, replace_na(this_kurtosis), 
                  replace_na(norm_test(seas_obj, "kurtosis")))
        }
        this_skewness <- get_norm_stat(seas_obj, "skewness")
        if (!is.null(this_skewness)) {
            this_summary <- 
                c(this_summary, replace_na(this_skewness), 
                  replace_na(norm_test(seas_obj, "skewness")))
        }
    }
    
    # add automatic outliers, if specified
    if (add_auto_out) {
        this_summary <- c(this_summary, this_autoout, this_autoreg)
    }
    
    # create summary list, if requested
    if (return_list) {
        this_summary_list <- vector("list", length(this_summary))
        for (i in 1:length(this_summary)) {
            this_summary_list[[i]] <- this_summary[i]
        }
               
        # add seasonal, if specified
        if (add_seasonal) {
            this_names <- c(this_names, "Seasonal_QS")
        }
        
        # add peaks, if specified
        if (add_spec) {
            this_names <- c(this_names, "Spec_Peaks")
        }
        
        # add aicc, if specified
        if (add_aicc) {
            this_names <- c(this_names, "AICC")
        }
        
        # add normality statistics, if specified
        if (add_norm) {
            if (!is.null(this_a)) {
                this_names <- c(this_names, "Gearys_a", "a_test")
            }
            if (!is.null(this_kurtosis)) {
                this_names <- c(this_names, "Kurtosis", "Kurtosis_test")
            }
            if (!is.null(this_skewness)) {
                this_names <- c(this_names, "Skewness", "Skewness_test")
            }
        }
        
        # add automatic outliers, if specified
        if (add_auto_out) {
            this_names <- c(this_names, "Number_of_AutoOut", "Auto_Outliers")
        }
        
        # set element names for list
        names(this_summary_list) <- this_names
        
        # convert some elements from characters to numbers.
        this_summary_list$Number_of_Reg <- as.numeric(this_summary_list$Number_of_Reg)
        if (add_aicc) {
            this_summary_list$AICC <- as.numeric(this_summary_list$AICC)
        }
        if (add_norm) {
            if (!is.null(this_a)) {
                this_summary_list$Gearys_a <- as.numeric(this_summary_list$Gearys_a)
            }
            if (!is.null(this_kurtosis)) {
                this_summary_list$Kurtosis <- as.numeric(this_summary_list$Kurtosis)
            }
            if (!is.null(this_skewness)) {
                this_summary_list$Skewness <- as.numeric(this_summary_list$Skewness)
            }
        }
        if (add_auto_out) {
            this_summary_list$Number_of_AutoOut <- 
                as.numeric(this_summary_list$Number_of_AutoOut)
        }
        
        # return list
        return(this_summary_list)
    }
    
    return(this_summary)
    
}
