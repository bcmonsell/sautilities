#' AR(30) Spectrum Critical Value
#'
#' Generate critical values for AR(30) spectrum as in Maravall (2012).
#'
#' Version 1.5, 5/25/2023
#'
#' @param n_sim integer scalar; number of simulations; default is 100000
#' @param series_length integer scalar; length of each series simulated.
#'        Default is 121.
#' @param freq integer scalar; frequency of the time series; default is 12 (monthly). 
#' @return List of critical values for each seasonal frequency for the 95th and 99th percentile.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples 
#' ar30_spec_cv <- gen_ar_spec_cv(1000, 97, 12)
#' @export
gen_ar_spec_cv <- function(n_sim = 100000, series_length = 121, freq = 12) {
    # Author: Brian C. Monsell (OEUS) Version 1.5, 5/25/2023
   if (freq == 12) {
       f_index <- c(11, 21, 31, 41, 51, 61)
       f_num   <- 6
   } else {
       f_index <- c(31, 61)
       f_num   <- 2
   }
   rw_matrix <- matrix(0, ncol = f_num, nrow = n_sim)
   
   for (i in 1:n_sim) {
      this_sim <- rnorm(series_length)
      this_ar_spec <- spec.ar(this_sim, 61, 30, plot = FALSE)
      
      median_spec  <- median(this_ar_spec$spec)
      range_spec   <- max(this_ar_spec$spec) - min(this_ar_spec$spec)
      
      for (j in 1:(f_num-1)) {
          j_index <- f_index[j]
          if (this_ar_spec$spec[j_index] > median_spec) {
              rw_matrix[i,j] <- 
                 min(this_ar_spec$spec[j_index] - this_ar_spec$spec[j_index-1],
                     this_ar_spec$spec[j_index] - this_ar_spec$spec[j_index+1]) / range_spec
          }     
      }
      j_index <- f_index[f_num]
      if (this_ar_spec$spec[j_index] > median_spec) {
          rw_matrix[i,f_num] <- 
              (this_ar_spec$spec[j_index] - this_ar_spec$spec[j_index-1]) / range_spec
      }  
   }
   
   cv_95 <- array(0, dim = f_num)
   cv_99 <- array(0, dim = f_num)
   
   for (i in 1:f_num) {
       this_freq <- rw_matrix[,i]
       cv_95[i] <- quantile(this_freq, probs = 0.95)
       cv_99[i] <- quantile(this_freq, probs = 0.99)
   }
   
   return(list(cv95 = cv_95, cv99 = cv_99))
}