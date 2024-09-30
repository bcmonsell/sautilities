#' Generate regression coefficient summary
#'
#' Generate a summary of regression coefficients for a single series 
#'
#' Version 3.4, 5/14/2024
#'
#' @param seas_obj \code{seas} object generated from a call of \code{seas} on a single time series 
#'        This is a required entry.
#' @param this_xreg_names Character array; name of user defined regressors.  
#'        Default is \code{NULL}, no user defined regressors.
#' @return matrix of regression coefficients, standard errors, and t-statistics for a given series
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' air_seas <- seasonal::seas(AirPassengers, x11="", slidingspans = "", 
#'                            transform.function = "log", arima.model = "(0 1 1)(0 1 1)", 
#'                            regression.aictest = 'td', forecast.maxlead=36, 
#'                            check.print = c( "pacf", "pacfplot" ))
#' air_reg_matrix <- get_regression_estimates_matrix(air_seas)
#' @export
get_regression_estimates_matrix <- function(seas_obj = NULL, this_xreg_names = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 3.4, 5/14/2024
    
    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
        if (!inherits(seas_obj, "seas")) {
           stop("First argument must be a seas object")
        }
    }
    
    # Get UDG diagnostic output
    this_udg <- seasonal::udg(seas_obj)
    
    # Get number of regression coefficients, return if zero
    this_nreg <- as.numeric(this_udg["nreg"])
    if (this_nreg == 0) {
        return(NULL)
    }
    
    # Get the position of the \code{nreg} code in UDG data
    this_reg_index <- get_udg_index(this_udg, "nreg")
    
    # Get keywords for regression variables
    this_reg_names <- names(this_udg)[seq(1, this_nreg) + this_reg_index]
    
    # Get names for regression variables
    reg_vec <- unlist(strsplit(this_reg_names, "[$]"))[seq(2, 2 * this_nreg, 2)]
    
    # if names for the user-defined regressors are supplied, set number of 
    # user defined regressor names
    if (!is.null(this_xreg_names)) {
        num_xreg_names <- length(this_xreg_names)
        
        # generate filter indicating which regressors are user defined regressors 
        # from the UDG output
        if (num_xreg_names > 1) {
            xreg_filter <- reg_vec %in% paste("xreg", seq(1, num_xreg_names), sep = "")
        } else {
            xreg_filter <- reg_vec %in% "xreg"
            if (sum(xreg_filter) == 0) {
                xreg_filter <- reg_vec %in% "xreg1"
            }
        }
        
        # if more than one user defined regressor, check to see if the number of 
        # user defined regressors match the number of names, then set the names of 
        # the user defined regressors else print out warning message
        if (sum(xreg_filter) > 0) {
            if (sum(xreg_filter) == num_xreg_names) {
                reg_vec[xreg_filter] <- this_xreg_names
            } else {
                warning("Number of names for user-defined regressors doesn't match number of user-defined regressors")
            }
        }
    }
    
    # Initialize vector used to store regression information
    this_vector <- vector("numeric", length = 3 * length(this_reg_names))
    
    # Process UDG output for each coefficient
    for (i in 1:length(this_reg_names)) {
        # Get name of ARMA coefficient, UDG information
        this_name <- this_reg_names[i]
        this_entry <- this_udg[this_name]
        
        # generate pointers for \code{this_vector}
        i1 <- (i - 1) * 3 + 1
        i2 <- i * 3
        
        # if number of elements in \code(this_entry} is one, split entry at spaces and 
        # store in \code{this_vector}, otherwise load information into \code{this_vector}
        if (length(unlist(this_entry)) == 1) {
            this_entry_vec <- strsplit(unlist(this_entry), split = " ")
            this_vector[i1:i2] <- unlist(this_entry_vec)[1:3]
        } else {
            this_vector[i1:i2] <- unlist(this_entry)
        }
    }
    
    # Form matrix from information extracted from UDG
    this_reg_matrix <- matrix(as.numeric(this_vector), ncol = 3, byrow = TRUE, 
                              dimnames = list(reg_vec, c("Coefficient", "Std_Err", "T-value")))
    
    # return regression matrix
    return(this_reg_matrix)
    
}
