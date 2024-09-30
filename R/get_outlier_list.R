#' List of outliers generated from a list of seasonal objects
#'
#' Generate a summary of model outliers from a list of \code{seas} objects series.
#'
#' Version 2.3, 5/7/2024
#'
#' @param seas_obj_list list of \code{seas} objects generated from a call of \code{seas} 
#'        on a single time series. A required argument.
#' @return vector of model diagnostics for a given series
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
#'                   check.print = c( "pacf", "pacfplot" ))
#' unemp_seas_update <- 
#'      Filter(function(x) inherits(x, "seas"), unemp_seas_list)
#' unemp_diag <- 
#'     get_outlier_list(unemp_seas_update)
#' @export
get_outlier_list <- function(seas_obj_list = NULL) {
  # Author: Brian C. Monsell (OEUS) Version 2.3, 5/7/2024

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
  
  all_outlier_types <- c("ao", "ls", "tc", "rp", "so", "tl", "qi", "qd")
  two_outlier_types <- c("rp", "tl", "qi", "qd")
  
  out_label_month   <- tolower(month.abb)
  out_label_quarter <- c("q1", "q2", "q3", "q4")
  
  # save number of elements in \code{seas_obj_list}
  nList <- length(seas_obj_list_update)
  all_list_names <- names(seas_obj_list_update)
  
  # initialize all the elements
  outlier_name_vec    <- vector(mode = "character")
  outlier_type_vec    <- vector(mode = "character")
  outlier_auto_vec    <- vector(mode = "character")
  outlier_tstat_vec   <- vector(mode = "numeric")
  outlier_coef_vec    <- vector(mode = "numeric")
  outlier_year_vec    <- vector(mode = "integer")
  outlier_period_vec  <- vector(mode = "integer")
  outlier_year2_vec   <- vector(mode = "integer")
  outlier_period2_vec <- vector(mode = "integer")
  # outlier_tcalpha_vec <- vector(mode = "numeric")
  
  # process each element of the list
  iOut <- 1
  # contains_tc   <- FALSE
  contains_two  <- FALSE
  contains_auto <- FALSE
  for (i in 1:nList) {
    this_seas <- seas_obj_list_update[[i]]
    this_freq <- seasonal::udg(this_seas, "freq")
    this_name <- all_list_names[i]
    nOut <- seasonal::udg(this_seas, "outlier.total")
    if (nOut > 0) {
      this_udg <- seasonal::udg(this_seas)
      names_udg <- names(this_udg)
      nReg <- seasonal::udg(this_seas, "nreg")
      iReg <- sautilities::get_udg_index(this_udg, "nreg") + 1
      lReg <- iReg + nReg - 1
      for (j in iReg:lReg) {
        this_key_vec <- 
          unlist(strsplit(names_udg[j], 
                          split = '[$]'), 
                 use.names = FALSE)
        # check if this an automatically identified outlier
        if (this_key_vec[1] == "AutoOutlier") {
          outlier_auto_vec <- c(outlier_auto_vec, "yes")
          this_outlier <- tolower(this_key_vec[2])
          is_outlier <- "yes"
          if (!contains_auto) { contains_auto <- TRUE }
        } else {
          this_reg  <- tolower(this_key_vec[2])
          this_type <- substr(this_reg, 1, 2)
          if (sum(all_outlier_types %in% this_type) > 0) {
            outlier_auto_vec <- c(outlier_auto_vec, "no")
            this_outlier <- this_reg
            is_outlier <- "yes"
          } else {
            is_outlier <- "no"
          }
        }
        
        # if regressor is outlier, finish processing
        if (is_outlier == "yes") {
          outlier_name_vec    <- c(outlier_name_vec, this_name)
          this_est            <- this_udg[[names_udg[j]]]
          outlier_tstat_vec   <- c(outlier_tstat_vec, this_est[3])
          outlier_coef_vec    <- c(outlier_coef_vec, this_est[1])
           
          this_otl_list <- 
               sautilities::proc_outlier(this_outlier, this_freq)
          outlier_type_vec    <- c(outlier_type_vec, this_otl_list$type)
          outlier_year_vec    <- c(outlier_year_vec, this_otl_list$year)
          outlier_period_vec  <- c(outlier_period_vec, this_otl_list$period)
          if (sum(two_outlier_types %in% this_otl_list$type) > 0) {
            outlier_year2_vec    <- c(outlier_year2_vec, this_otl_list$year2)
            outlier_period2_vec  <- c(outlier_period2_vec, this_otl_list$period2)
            if (!contains_two) { contains_two <- TRUE }
          }
          # if (this_otl_list$type == "tc") {
          #   outlier_tcalpha_vec  <- c(outlier_tcalpha_vec, this_seas$model$regression$tcrate)
          #   if (!contains_tc) { contains_tc <- TRUE }
          # } else {
          #   outlier_tcalpha_vec  <- c(outlier_tcalpha_vec, 0.0)
          # }
        }
      }
    }
  }
  
  outlier_list_df <- data.frame(
      name =   outlier_name_vec,
      type =   outlier_type_vec,
      tstat =  outlier_tstat_vec,   
      coef =   outlier_coef_vec,    
      year =   outlier_year_vec,   
      period = outlier_period_vec 
      )
  if (contains_two) {
    outlier_list_df$year2 =   outlier_year2_vec   
    outlier_list_df$period2 = outlier_period2_vec 
  }
  # if (contains_tc) {
  #   outlier_list_df$tcalpha = outlier_tcalpha_vec
  # }
  if (contains_auto) {
    outlier_list_df$auto =    outlier_auto_vec    
  }
  return(outlier_list_df)
}