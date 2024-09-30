#' Non-Parametric test from Maravall (2012)
#'
#' Non-Parametric test for seasonality based on Kendall and Ord (1990), and
#' originally due to Friedman from a paper by Maravall.
#' This code is adapted from \code{kendalls} subroutine in \code{ansub11.f} 
#' from the \code{X-13ARIMA-SEATS} source code
#'
#' Version 1.7, 5/23/2023
#'
#' @param x \code{ts} time series object. This is a required entry.
#' @return List object with three elements: \code{ken} (test statistic), \code{df} 
#'         (degrees of freedom), \code{cv} (test probability)
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' NP_test_air <- NP_test(AirPassengers)
#' @export
NP_test <- function(x = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 1.7, 5/23/2023
   if (is.null(x)) {
       stop(" Must specify time series")
   }
   mq <- frequency(x)
   if (mq <= 1) {
       stop(" Cannot be used with annual series")
   }
   
   nz = length(x)
   ny = nz/mq
   res = nz - ny*mq
   dbl_max = 10^306
   
   obs <- array(0, dim = mq)
   loopmax = 1000
   this_tiny = 10^(-19)
   r = matrix(0, ncol=mq, nrow=ny)
   
   for (i in 1:ny) {
       for (j in 1:mq) {
           j_index <- res + (i-1) * mq + j
           obs[j] = x[j_index]
       }
       ind = 1
       maxloop = 0
       this_loop <- TRUE
       while (this_loop) {
           maxloop = maxloop +1
           min_val = min(obs)
           k = 0
           found <- array(0, dim = mq)
           for (j in 1:mq) {
               if (abs(obs[j]-min_val) < this_tiny) {
                   k = k + 1
                   found[j] = 1
               }
           }
           value = ind + (k-1) / 2.0
           obs[found == 1] = dbl_max
           for (j in 1:mq) {
               if (found[j] == 1) {
                   obs[j] = dbl_max
                   r[i,j] = value
               }
           }
           ind = ind + k
           this_loop = (ind <= mq) && (maxloop < loopmax)
       }
       if (maxloop > loopmax) {
           kendalls_list <- list(ken = 0.0, df = mq-1, cv = 0)
           return(kendalls_list)
       }
   }

   m <- array(0, dim = mq)
   for (i in 1:mq) {
       sum = 0.0
       for (j in 1:ny) {
           sum = sum + r[j,i]
       }
       m[i] = sum
   }

   tmp = 0.0
   for (i in 1:mq) {
       tmp = tmp + (m[i]-ny*(mq+1)/2.0) * (m[i]-ny*(mq+1)/2.0)
   }
      
   this_kendalls = 12.0*tmp / ((mq+1)*mq*ny)
   kendalls_list <- list(ken = this_kendalls, 
                         df = mq-1, 
                         cv = pchisq(this_kendalls, mq-1))
   return(kendalls_list)
}
