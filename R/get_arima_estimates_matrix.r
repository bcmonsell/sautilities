#' ARMA Coefficient Summary 
#'
#' Generate a summary of ARMA coefficients for a single series.
#'
#' Version 3.2, 5/14/2024
#'
#' @param seas_obj \code{seas} object generated from a call of \code{seas} on a single time series. 
#'        This is a required entry.
#' @param add_diff logical scalar; add differencing information, if included in model
#' @return matrix of ARMA coefficients, standard errors, and t-statistics for a given series
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' air_seas <- 
#'    seasonal::seas(AirPassengers, x11="", slidingspans = "", transform.function = "log",
#'                         arima.model = "(0 1 1)(0 1 1)", regression.aictest = 'td', 
#'                         forecast.maxlead=36, check.print = c( "pacf", "pacfplot" ))
#' air_arima_matrix <- get_arima_estimates_matrix(air_seas, add_diff = TRUE)
#' @export
get_arima_estimates_matrix <- function(seas_obj = NULL, add_diff = FALSE) {
    # Author: Brian C. Monsell (OEUS) Version 3.2, 5/14/2024
    
    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
           stop("First argument must be a seas object")
        }
    }
    
    # Get UDG diagnostic output
    this_udg <- seasonal::udg(seas_obj)
    
    # Get number of ARMA coefficients, return if zero
    this_nmodel <- as.numeric(this_udg["nmodel"])
    if (this_nmodel == 0) {
        return(NULL)
    }
    
    # Get the position of the \code{nmodel} code in UDG data
    this_model_index <- get_udg_index(this_udg, "nmodel")
    
    # Get names of ARMA coefficients
    this_model_names <- names(this_udg)[seq(1, this_nmodel) + this_model_index]
    
    # Substitute '_' for '$' for ARMA names
    model_vec <- gsub("\\$", "_", this_model_names)
    
    # Initialize vector used to store ARMA information
    this_vector <- vector("numeric", length = 3 * length(this_model_names))
    
    # Process UDG output for each coefficient
    for (i in 1:length(this_model_names)) {
        # Get name of ARMA coefficient, UDG information
        this_name <- this_model_names[i]
        this_entry <- this_udg[this_name]
        
        # generate pointers for \code{this_vector}
        i1 <- (i - 1) * 3 + 1
        i2 <- i * 3
        
        # if number of elements in \code(this_entry} is one, split entry at spaces and store in \code{this_vector},
        # otherwise load information into \code{this_vector}
        if (length(unlist(this_entry)) == 1) {
            this_entry_vec <- strsplit(unlist(this_entry), split = " ")
            this_vector[i1:i2] <- unlist(this_entry_vec)[1:3]
        } else {
            this_vector[i1:i2] <- unlist(this_entry)
        }
    }
    
    # Form matrix from information extracted from UDG
    this_model_matrix <- 
        matrix(as.numeric(this_vector), ncol = 3, byrow = TRUE, 
               dimnames = list(model_vec, c("Coefficient", "Std_Err", "T-value")))
    
    # add differencing information, if requested
    if (add_diff) {
        # Extract number of seasonal, nonseasonal differences
        this_nseas_diff <- this_udg[[this_model_index - 2]]
        this_seas_diff <- this_udg[[this_model_index - 1]]
        
        # update names of model coefficients
        model_vec <- c("Nonseasonal_Diff", "Seasonal_Diff", model_vec)
        
        # update matrix with model information with differencing information
        this_model_matrix <- 
            rbind(c(this_nseas_diff, 0, 0), c(this_seas_diff, 0, 0), this_model_matrix)
        
        # update row dimension names
        dimnames(this_model_matrix)[[1]] <- model_vec
    }
    
    # return model matrix
    return(this_model_matrix)
    
}
