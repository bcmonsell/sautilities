#' Import regCMPNT estimates file
#'
#' Reads in an estimated component from a file saved by regCMPNT
#'
#' Version 1.2, 12/12/2024
#'
#' @param file_name Character string; file name for regCMPNT estimate file.
#'        This is a required entry
#' @param column_name Array of character strings; names for the columns of the 
#'        estimates matrix. Array must be of length 5.
#'        Default is \code{c("Unscaled_Stochastic", "Scale_Factors", "Scaled_Stochastic", 
#'        "Regression_Effects", "Combined_Estimate")}.
#' @param return_matrix Logical scalar; determines if a matrix or data frame object is returned.
#'        Default is \code{TRUE}.
#' @return A \code{ts} matrix object or a data frame of \code{ts} objects which contains 
#'         the contents of the estimates for a given component from a regCMPNT run. 
#'         The file name for the for the component file has an \code{.est} file extension.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' \dontrun{
#' n3000019_trend_df         <- import_est("n300019_rev2_comp01.est", return_matrix = FALSE)
#' n3000019_seasonal_df      <- import_est("n300019_rev2_comp02.est", return_matrix = FALSE)
#' n3000019_irregular_df     <- import_est("n300019_rev2_comp03.est", return_matrix = FALSE)
#' n3000019_samplingerror_df <- import_est("n300019_rev2_comp04.est", return_matrix = FALSE)
#' }
#' @export
import_est <- 
	function(file_name = NULL,
             column_name = NULL,
			 return_matrix = TRUE) {
    # Author: Brian C. Monsell (OEUS) Version 1.2, 12/12/2024
	if (is.null(file_name)) {
		stop("Must specify valid file name in first argument")
	}
	
	if (is.null(column_name)) {
		column_name <- 
			c("Unscaled_Stochastic", "Scale_Factors", "Scaled_Stochastic", 
			  "Regression_Effects", "Combined_Estimate")
	} else {
		if (length(column_name) != 5) {
			stop("Must specify exactly 5 column names.")
		}
	}
	
	this_table <- read.table(file_name)
	
	this_period <- (this_table[,1] * 100) %% 100
	this_year   <- this_table[,1] %/% 1
	this_freq   <- max(this_period)
	this_start  <- c(this_year[1], this_period[1])
	
	ncol_table <- ncol(this_table)
	
	if (return_matrix) {
		final_matrix <- 
			ts(this_table[,2:ncol_table], start = this_start, frequency = this_freq)
		colnames(final_matrix) <- column_name
		return(final_matrix)
	} else {
		final_df <-
			as.data.frame(lapply(this_table[,2:ncol_table], ts, start = this_start, frequency = this_freq))
		colnames(final_df) <- column_name
		return(final_df)
	}
}