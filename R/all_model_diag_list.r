#' Model diagnostic summary from a list
#'
#' Generate a summary of model diagnostics from a list of \code{seas} objects series.
#'
#' Version 8.0, 6/21/2024
#'
#' @param seas_obj_list list of \code{seas} objects generated from a call of \code{seas} 
#'        on a single time series. A required argument.
#' @param add_aicc logical scalar; add AICC value to the summary. 
#'        Default is \code{TRUE}
#' @param add_norm logical scalar; add normality statistics to the summary. 
#'        Default is \code{FALSE}
#' @param add_auto_out logical scalar; add identified automatic outliers to the summary. 
#'        Default is \code{FALSE}
#' @param add_seasonal logical scalar; add QS test for seasonality to the summary. 
#'        Default is \code{FALSE}.
#' @param add_spec logical scalar; add test for spectral peaks to the summary. 
#'        Default is \code{FALSE}
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
#'        Default is \code{TRUE}
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
#' @param return_data_frame logical scalar; if \code{TRUE}, return a data frame of the diagnostics, 
#'        otherwise return a matrix. Default is \code{FALSE}.
#' @return matrix or data frame of model diagnostics for a given set of series series
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' unemp_seas_list <- 
#'    seasonal::seas(unemployment_list, slidingspans = "", 
#'                   transform.function = "log", 
#'                   outlier.types = "all",
#'                   arima.model = "(0 1 1)(0 1 1)",
#'                   forecast.maxlead=36, x11 = "",
#'                   check.print = c( "pacf", "pacfplot" ), 
#'                   check.save = "acf")
#' unemp_seas_update <- 
#'      Filter(function(x) inherits(x, "seas"), unemp_seas_list)
#' unemp_diag <- 
#'     all_model_diag_list(unemp_seas_update, add_aicc = TRUE, 
#'                         add_norm = TRUE, add_auto_out = TRUE, 
#'                         add_spec = TRUE, add_pacf = FALSE, qs_test_span = TRUE)
#' @export
all_model_diag_list <- function(seas_obj_list = NULL, add_aicc = FALSE, add_norm = FALSE, 
    add_auto_out = FALSE, add_seasonal = FALSE, add_spec = FALSE, add_lbq = TRUE, 
   set_lbq_lags_fail = c(12, 24), set_lbq_p_limit = 0.01, set_acf_num_sig = 8, 
    set_acf_lags_fail = c(1, 2, 3, 4, 12, 24), set_acf_lags_warn = c(12, 24), 
    add_pacf = TRUE, qs_test_span = FALSE, set_qs_p_limit_pass = 0.01, 
    set_qs_p_limit_warn = 0.05, set_qs_robust_sa = TRUE, set_spec_peak_level = 6, 
    set_spec_peak_warn = 3, return_data_frame = FALSE) {
    # Author: Brian C. Monsell (OEUS) Version 8.0, 6/21/2024
    
    # check if a value is specified for \code{seas_obj_list}
    if (is.null(seas_obj_list)) {
        stop("must specify a list of seasonal objects")
    } else {
        if (!is.list(seas_obj_list)) {
            stop("must specify a list")
        }
    }
    
    # filter elements of the list that are not seas objects
    seas_obj_list_update <-
      Filter(function(x) inherits(x, "seas"), seas_obj_list)
      
    # get ARIMA Model
    this_model <- 
        lapply(seas_obj_list_update, function(x) try(seasonal::udg(x, "arimamdl")))
    
    # get number of regressors
    this_nreg <- 
        lapply(seas_obj_list_update, function(x) try(seasonal::udg(x, "nreg")))
    
    # add automatic outliers, if specified
    if (add_auto_out) {
        this_autoout <- 
           lapply(seas_obj_list_update, function(x) try(seasonal::udg(x, "autoout")))
        this_autoreg <- 
           lapply(seas_obj_list_update, function(x) try(get_auto_outlier_string(x)))
    }
    
    # add aicc, if specified
    if (add_aicc) {
        this_aicc <- 
            lapply(seas_obj_list_update, function(x) try(seasonal::udg(x, "aicc")))
    }
    
    # generate residual ACF test result
    if (add_lbq) {
        this_lbq_test <- 
             lapply(seas_obj_list_update, function(x) try(lbq_test(x, 
                   lbq_lags_fail = set_lbq_lags_fail, p_limit = set_lbq_p_limit,
                   return_this = "why")))
                   
        # create summary matrix by binding different test results
        # set dimensions name for summary matrix
        this_model_summary <- 
            cbind(unlist(this_model), unlist(this_nreg), unlist(this_lbq_test))
        dimnames(this_model_summary)[[1]] <- names(this_model)
        dimnames(this_model_summary)[[2]] <- 
            c("ARIMA_model", "Number_of_reg", "LBQ_test")
   } else {
        this_acf_test <- 
            lapply(seas_obj_list_update, function(x) try(acf_test(x, 
                   num_sig = set_acf_num_sig, acf_lags_fail = set_acf_lags_fail, 
                   acf_lags_warn = set_acf_lags_warn, include_pacf = add_pacf, 
                   return_this = "why")))
                   
        # create summary matrix by binding different test results
        # set dimensions name for summary matrix
        this_model_summary <- 
            cbind(unlist(this_model), unlist(this_nreg), unlist(this_acf_test))
        dimnames(this_model_summary)[[1]] <- names(this_model)
        dimnames(this_model_summary)[[2]] <- 
            c("ARIMA_model", "Number_of_reg", "ACF_test")
    }
    
        
    # generate seasonality test using QS
    if (add_seasonal) {
        old_dimnames <- dimnames(this_model_summary)[[2]]
        if (qs_test_span) {
            this_seasonal_test <- 
                lapply(seas_obj_list_update, function(x) try(qs_seasonal_test(x, test_full = TRUE, 
                       test_span = FALSE, p_limit_pass = set_qs_p_limit_pass, 
                       p_limit_warn = set_qs_p_limit_warn, 
                       robust_sa = set_qs_robust_sa, return_this = "why")))
        } else {
            this_seasonal_test <- 
                lapply(seas_obj_list_update, function(x) try(qs_seasonal_test(x, test_full = FALSE, 
                       test_span = TRUE, p_limit_pass = set_qs_p_limit_pass, 
                       p_limit_warn = set_qs_p_limit_warn, 
                       robust_sa = set_qs_robust_sa, return_this = "why")))
        }
        this_model_summary <- cbind(this_model_summary, unlist(this_seasonal_test))
        dimnames(this_model_summary)[[2]] <- c(old_dimnames, "Seasonal_QS")
    }
    
    # generate seasonality test using spectral peaks
    if (add_spec) {
        old_dimnames <- dimnames(this_model_summary)[[2]]
        this_spec_peak_test <- lapply(seas_obj_list_update, function(x) 
            try(spec_peak_test(x, peak_level = set_spec_peak_level, peak_warn = set_spec_peak_warn, 
                               this_spec = "spcori", return_this = "why")))
        this_model_summary <- cbind(this_model_summary, unlist(this_spec_peak_test))
        dimnames(this_model_summary)[[2]] <- c(old_dimnames, "Spec_peaks")
    }
    
    
    # add aicc to summary matrix, if specified
    if (add_aicc) {
        old_dimnames <- dimnames(this_model_summary)[[2]]
        this_model_summary <- cbind(this_model_summary, unlist(this_aicc))
        dimnames(this_model_summary)[[2]] <- c(old_dimnames, "AICC")
    }
    
    # generate normality statistics, if specified, and add to summary matrix
    if (add_norm) {
        old_dimnames <- dimnames(this_model_summary)[[2]]
        this_a <- lapply(seas_obj_list, function(x) try(get_norm_stat(x, "a")))
        this_a_test <- lapply(seas_obj_list, function(x) try(norm_test(x, "a")))
        this_kurtosis <- lapply(seas_obj_list, function(x) try(get_norm_stat(x, "kurtosis")))
        this_kurtosis_test <- lapply(seas_obj_list, function(x) try(norm_test(x, "kurtosis")))
        this_skewness <- lapply(seas_obj_list, function(x) try(get_norm_stat(x, "skewness")))
        this_skewness_test <- lapply(seas_obj_list, function(x) try(norm_test(x, "skewness")))
        this_model_summary <- cbind(this_model_summary, replace_na(unlist(this_a)), 
                                    replace_na(unlist(this_a_test)), 
                                    replace_na(unlist(this_kurtosis)), 
                                    replace_na(unlist(this_kurtosis_test)), 
                                    replace_na(unlist(this_skewness)), 
                                    replace_na(unlist(this_skewness_test)))
        dimnames(this_model_summary)[[2]] <- 
            c(old_dimnames, "Gearys_a", "a_test", "Kurtosis", "Kurtosis_test", 
                            "Skewness", "Skewness_test")
        
    }
    
    # add automatic outliers to summary matrix, if specified
    if (add_auto_out) {
        old_dimnames <- dimnames(this_model_summary)[[2]]
        this_model_summary <- 
            cbind(this_model_summary, unlist(this_autoout), unlist(this_autoreg))
        dimnames(this_model_summary)[[2]] <- 
            c(old_dimnames, "Number_of_autoOut", "Auto_outliers")
    }
    
    if (return_data_frame) {
        this_model_frame <- data.frame(this_model_summary)
        if (!is.null(this_model_frame$Number_of_reg)) {
            this_model_frame$Number_of_reg <- as.integer(this_model_frame$Number_of_reg)
        }
        if (!is.null(this_model_frame$Number_of_autoOut)) { 
            this_model_frame$Number_of_autoOut <- as.integer(this_model_frame$Number_of_autoOut)
        }
        if (!is.null(this_model_frame$Gearys_a)) { 
            this_model_frame$Gearys_a <- as.numeric(this_model_frame$Gearys_a)
        }
        if (!is.null(this_model_frame$Kurtosis)) { 
            this_model_frame$Kurtosis <- as.numeric(this_model_frame$Kurtosis)
        }
        if (!is.null(this_model_frame$Skewness)) { 
            this_model_frame$Skewness <- as.numeric(this_model_frame$Skewness)
        }
        if (!is.null(this_model_frame$AICC)) { 
            this_model_frame$AICC <- as.numeric(this_model_frame$AICC)
        }
        return(this_model_frame)
    } else {
    # return summary matrix
        return(this_model_summary)
    }
    
}
