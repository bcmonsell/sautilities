#' Convert date string from UDG output
#'
#' Convert a date string from the output of the \code{udg} function from the 
#' \code{seasonal} package \insertCite{seasonal2018}{sautilities} 
#' to a \code{c(year, month)} date. 
#'
#' Version 2.0, 2026-02-24
#'
#' @param this_date_string Date string usually extracted from the X-13 UDG output.
#' @return Integer array of length 2 with the year and month/quarter of from the date string.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples 
#' air_seas <- 
#'     seasonal::seas(AirPassengers, 
#'                    series.save = "b1",
#'                    transform.function = "log",
#'                    arima.model="(0 1 1)(0 1 1)",
#'                    forecast.maxlead = 36, 
#'                    slidingspans = "")
#' this_start_spec_string <- seasonal::udg(air_seas, "startspec")
#' this_start_spec        <- 
#'     convert_date_string_to_date(this_start_spec_string)
#' @importFrom Rdpack reprompt
#' @export
convert_date_string_to_date <- function(this_date_string) {
    # Author: Brian C. Monsell (OEUS) Version 2.0, 2026-02-24
    this_date_vec <- unlist(stringr::str_extract_all(this_date_string, "[0-9]"))
    this_vec_length <- length(this_date_vec)
    
    this_year   <- as.numeric(paste0(this_date_vec[(this_vec_length-3):this_vec_length], collapse = ""))
    this_period <- as.numeric(paste0(this_date_vec[1:(this_vec_length-4)], collapse = ""))
    
    return(c(this_year, this_period))
}