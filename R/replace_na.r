#' Replace NA
#'
#' Replace NA in a vector with a string or number.
#'
#' Version 2.4, 5/25/2023
#'
#' @param this_vec Vector object. This is a required entry.
#' @param replace_string Character scalar which replaces the NAs in the vector. 
#'        Default is \code{'NA'}.
#' @param replace_number Number which replaces the \code{NA}s in the vector. 
#'        Default is the \code{NA} is replaced by the string in \code{replace_string}.
#' @return A vector with all \code{NA}s replaced by either a character string or a number.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' sample_vec <- c(rnorm(25), NA, rnorm(24))
#' sample_vec_missing  <- replace_na(sample_vec, replace_string = 'Missing')
#' sample_vec_missing_number  <- replace_na(sample_vec, replace_number = -9999)
#' @export
replace_na <- function(this_vec = NULL, replace_string = "NA", replace_number = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 2.4, 5/25/2023
    
    # check if a value is specified for \code{this_vec}
    if (is.null(this_vec)) {
        stop("must specify a vector.")
    }

    # process each element of the vector, checking for NAs
    for (i in 1:length(this_vec)) {
        if (is.na(this_vec[i])) {
            if (is.null(replace_number)) {
    # replace NAs with \code{replace_string}
                this_vec[i] <- replace_string
            } else {
    # replace NAs with \code{replace_number}
                this_vec[i] <- replace_number
            }
        }
    }
    
    return(this_vec)
}
