#' generate date object from days and year
#'
#' Generate a date object from the number of days from the beginning of the year 
#' and the target year. The \code{lubridate} package is used in this function
#' \insertCite{lubridate}{sautilities}.
#'
#' Version 2.0, 2026-03-04
#'
#' @param day_number Integer scalar, number of days from the start of the year. 
#'                   This is a required entry.
#' @param target_year Integer scalar, year of the date object.
#'                    This is a required entry.
#' @return Date object determined by the input arguments.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' this_start <- set_day_and_month(1, 2001)
#' this_end   <- set_day_and_month(350, 2025)
#' @importFrom Rdpack reprompt
#' @export
set_day_and_month <- 
   function(day_number = NULL, target_year= NULL) {
    # Author: Brian C. Monsell (OEUS), Version 2.0, 2026-03-04
	
    # check if a value is specified for \code{day_number}
    if (is.null(day_number)) {
        stop("nothing specified for day_number")
    }
    
    # check if a value is specified for \code{target_year}
    if (is.null(target_year)) {
        stop("no year specified for target_year")
    }
	
	base_date <- lubridate::ymd(paste0(target_year, "-01-01"))
	lubridate::yday(base_date) <- day_number
	
	return(base_date)
}