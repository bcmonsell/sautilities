#' Generate index of month abbreviation
#'
#' Process string of month abbreviation to return a numeric index .
#'
#' Version 2.4, 2020-09-18
#'
#' @param this_month_string Character string; 3 character abbreviation of month.
#'        This entry is required.
#' @return Index of month - 1 for \code{'Jan'}, 2 for \code{'Feb'}, etc.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' thisOtl <- 'AO2015.Jan'
#' thisCode <- 'AO'
#' thisPerChar <- substr(thisOtl,nchar(thisCode)+6,nchar(thisOtl))
#' thisPerIndex <- get_month_index(thisPerChar)
#' @export
get_month_index <- function(this_month_string = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 2.4, 2020-09-18

    if (is.null(this_month_string)) {
        stop("must specify the this_month_string argument")
    }
	
    # Initialize monthly abbreviations, index
    mAbb <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    mInd <- 1:12
    
    # return index of month abbreviation
    return(mInd[!is.na(match(tolower(mAbb), tolower(this_month_string)))])
}
