#' Import regCMPNT Variance file
#'
#' Reads in variances for a component from a file saved by regCMPNT
#'
#' Version 1.3, 12/12/2024
#'
#' @param file_name Character string; file name for regCMPNT variance file.
#'        This is a required entry
#' @param column_name Array of character strings; names for the columns of the 
#'        estimates matrix. Array must be of length 4.
#'        Default is \code{c("Unscaled_Stochastic", "Scaled_Stochastic", 
#'        "Regression_Estimation", "Combined")}.
#' @param return_matrix Logical scalar; determines if a matrix object is returned.
#'        Default is \code{TRUE}, which forces the function to return a data frame object.
#' @return A \code{ts} matrix object or a data frame of \code{ts} objects which contains 
#'         the contents of the variances for a given component from a regCMPNT run. 
#'         The file name for the for the component file has an \code{.var} file extension.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' \dontrun{
#' n3000019_trend_var_df         <- import_var("n300019_rev2_comp01.var", return_matrix = FALSE)
#' n3000019_seasonal_var_df      <- import_var("n300019_rev2_comp02.var", return_matrix = FALSE)
#' n3000019_irregular_var_df     <- import_var("n300019_rev2_comp03.var", return_matrix = FALSE)
#' n3000019_samplingerror_var_df <- import_var("n300019_rev2_comp04.var", return_matrix = FALSE)
#' }
#' @export
import_var <- 
	function(file_name = NULL,
             column_name = NULL,
			 return_matrix = TRUE) {
    # Author: Brian C. Monsell (OEUS) Version 1.3, 12/12/2024
	if (is.null(file_name)) {
		stop("Must specify valid file name in first argument")
	}
	
  this_table <- read.table(file_name)
  ncol_table <- ncol(this_table)
	
	if (is.null(column_name)) {
	  if (ncol_table == 4) {
	    column_name <- 
	      c("Unscaled_Stochastic", "Scaled_Stochastic", 
	        "Combined")
	  } else {
	    column_name <- 
	      c("Unscaled_Stochastic", "Scaled_Stochastic", 
	        "Regression_Estimation", "Combined")
	  }
	} else {
		if (length(column_name) != (ncol_table-1)) {
			stop(paste0("Must specify exactly ", (ncol_table-1), " 4 column names."))
		}
	}
	
	this_period <- (this_table[,1] * 100) %% 100
	this_year   <- this_table[,1] %/% 1
	this_freq   <- max(this_period)
	this_start  <- c(this_year[1], this_period[1])
	
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