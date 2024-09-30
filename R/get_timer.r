#' Generate elapsed time of run in miliseconds
#'
#' Read profiler information from saved files and generate the elapsed time of an X-13 run in miliseconds.
#'
#' Version 1.1, 11/29/2023
#'
#' @param this_base Character scalar; Filename of output file of the X-13 run.
#'        This is a required entry.
#' @param this_path Character scalar; Path of profiler output.
#'        This is an optional entry.
#' @return Numeric object of the elapsed time of the X-13 run.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' \dontrun{AE1011330000_time <- get_timer("AE1011330000_auto", "X:/code/census_build_60/basic/")} 
#' @export
get_timer <- function(this_base = NULL, this_path = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 1.1, 11/29/2023
    
    if (is.null(this_base)) {
        stop("must specify the name of output file of the X-13 run")
    }
    
    this_file <- paste0(this_base, "_Profiler.txt")
    if (!is.null(this_path)) {
        this_file <- paste0(this_path, this_file)
    }
    
    this_profiler <- 
        invisible(scan(this_file,
                       what = list(index = integer(), 
                                   label = "", 
                                   milisec = numeric()),
                       skip = 1))
    
    return(this_profiler$milisec[2] - this_profiler$milisec[1])
}