#' Get value from external UDG file
#'
#' parse external \code{udg} file and get value for a user supplied key
#'
#' Version 1.4, 12/12/2023
#'
#' @param this_base Character scalar; Filename of output file of the X-13 run.
#'        This is a required entry.
#' @param this_path Character scalar; Path of profiler output.
#'        This is an optional entry.
#' @param this_key Character scalar; Key from udg file to search for.
#'        This is a required entry.
#' @param return_numeric Logical scalar; Value returned is converted to numeric.
#'        Default is TRUE, returns value as a number.
#' @return Numeric value associated with \code{this_key}.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' \dontrun{this_ftest<-get_ftest_from_udg("AE1011330000_auto", 
#'                                         "X:/code/census_build_60/basic/",
#'                                         "aicc")}
#' @export
get_value_from_udg <- function(this_base = NULL, this_path = NULL, this_key = NULL,
                               return_numeric = TRUE) {
  # Author: Brian C. Monsell (OEUS) Version 1.4, 12/12/2023
  
  if (is.null(this_base)) {
    stop("must specify the name of output file of the X-13 run")
  }
  if (is.null(this_key)) {
    stop("must specify the key to extract from the udg file")
  }
  
  this_file <- paste0(this_base, ".udg")
  if (!is.null(this_path)) {
    this_file <- paste0(this_path, this_file)
  }
  
  con = file(this_file, "r")
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      close(con)
      cat(paste0("Cannot find associated value for ", this_key))
      return(NULL)
    }
    this_line_split = unlist(strsplit(line, ":"))
    if ( this_line_split[1] == this_key ) {
      this_value <- this_line_split[2]
      break
    }
  }
  
  close(con)
  
  if (return_numeric) {
    return(as.numeric(this_value))
  } else {
    this_nchar <- nchar(this_value)
    return(substring(this_value, 2, this_nchar))
  }
  
}