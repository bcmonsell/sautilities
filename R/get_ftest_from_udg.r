#' Get F-test info from external UDG file
#'
#' Parse \code{udg} file and get information for model-based F-test for a regressor.
#'
#' Version 1.1, 1/10/2024
#'
#' @param this_base Character scalar; Filename of output file of the X-13 run.
#'        This is a required entry.
#' @param this_path Character scalar; Path of \code{udg} file.
#'        This is an optional entry.
#' @param this_reg Character scalar; Type of regressor for model-based F-test.
#'        Default is \code{Seasonal}.
#' @return Numeric vector of the degrees of freedom, F-test, and p-value for this 
#'         model based F-test
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' \dontrun{this_ftest<-get_ftest_from_udg("AE1011330000_auto", "X:/code/census_build_60/basic/",
#'                                         "Trigonometric Seasonal")}
#' @export
get_ftest_from_udg <- function(this_base = NULL, this_path = NULL, this_reg = "Seasonal") {
  # Author: Brian C. Monsell (OEUS) Version 1.1, 1/10/2024
  
  if (is.null(this_base)) {
    cat("  must specify the name of output file of the X-13 run")
    return(NULL)
  }
  
  this_file <- paste0(this_base, ".udg")
  if (!is.null(this_path)) {
    this_file <- paste0(this_path, this_file)
  }
  
  this_key <- paste0("ftest$", this_reg)
  
  con = file(this_file, "r")
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      close(con)
      cat(paste0("Cannot find model based F-test for ", this_key))
      return(NULL)
    }
    this_line_split = unlist(strsplit(line, ":"))
    if ( this_line_split[1] == this_key ) {
      this_ftest <- this_line_split[2]
      break
    }
  }
  
  close(con)
  
  ftest_vec <- as.numeric(unlist(strsplit(this_ftest, " ")))
  return(ftest_vec[!is.na(ftest_vec)])
  
}