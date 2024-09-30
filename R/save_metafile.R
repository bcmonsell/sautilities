#' Generate X-13ARIMA-SEATS metafile
#'
#' Generates external metafile for spec files generated from a list of \code{seas} objects
#'
#' Version 3.11, 9/6/2023
#'
#' @param this_seas_list - list of seas objects the metafile will be generated from.
#'        This is a required entry.
#' @param this_name_vec vector of character string; vector of series names from the list of 
#'        seas objects that will be saved.
#'        Default is all elements of the seasonal object list \code{this_seas_list} are saved.
#' @param metafile_name - character string; base name of metafile to be generated. 
#'        If not specified, use name of list input as metafile name.
#'        Note - do not specify the \code{".mta"} file extension.
#' @param this_directory - optional directory where the meta file is stored. 
#'        If not specified, the metafile will be saved in the current working directory.
#' @param this_spec_directory - optional directory where the spec files are stored. 
#'        If not specified, the spec files are saved in the current working directory.
#' @param this_output_code - optional character code added to the end of the names in 
#'        \code{this_name_vec} to form the output name.
#'        If not specified, the metafile will not have alternate output file name(s).
#' @param this_output_directory - optional directory where the output files are stored. 
#'        If not specified, the output files are saved in the current working directory.
#' @param include_directory - logical scalar; if \code{TRUE}, include directory specified 
#'        in \code{this_directory} with file name output.
#'        Otherwise, output only names in \code{this_name_vec}. Default is \code{FALSE}. 
#'        Note that the argument \code{this_directory} must also be specified.
#' @return Generates metafile that can be used directly with the X-13ARIMA-SEATS program.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' unemp_seas_list <- seasonal::seas(unemployment_list, 
#'                       slidingspans = "", x11 = "",
#'                       arima.model = "(0 1 1)(0 1 1)",
#'                       transform.function = "log",
#'                       forecast.maxlead=36,
#'                       check.print = c( "pacf", "pacfplot" ))
#' unemp_seas_update <- 
#'      Filter(function(x) inherits(x, "seas"), unemp_seas_list)
#' \dontrun{save_metafile(unemp_seas_update, metafile_name = 'unemp',
#'               this_directory = 'X:\\seasonalAdj\\testing\\sautilities',
#'               this_spec_directory = 'X:\\seasonalAdj\\testing\\spc',
#'               this_output_directory = 'X:\\seasonalAdj\\testing\\out',
#'               include_directory = TRUE)}
#' @export
save_metafile <- function(this_seas_list = NULL, this_name_vec = NULL, 
                          metafile_name = NULL, this_directory = NULL, 
                          this_spec_directory = NULL, this_output_code = NULL, 
                          this_output_directory = NULL, include_directory = FALSE) {
    # Author: Brian C. Monsell (OEUS) Version 3.11, 9/6/2023

    # check if a value is specified for \code{this_seas_list}
    if (is.null(this_seas_list)) {
        stop("must specify a list")
    } else {
        if (!is.list(this_seas_list)) {
            stop("must specify a list")
        }
    }

    # Get list names
    if (is.null(this_name_vec)) {
        name_vec <- names(this_seas_list)
    } else {
        name_vec <- this_name_vec
    }
    name_vec_spec <- name_vec
    if (include_directory) {
        name_vec_spec <- paste(this_spec_directory, name_vec, sep = "\\")
    }

    # set up metafile name to be used to save list names, with directory information if provided
    if (is.null(metafile_name)) {
        file_name <- paste(deparse(substitute(this_seas_list)), ".mta", sep = "")
    } else {
        file_name <- paste(metafile_name, ".mta", sep = "")
    }
    if (!is.null(this_directory)) {
        file_name <- paste(this_directory, file_name, sep = "\\")
    }
    
    if (!is.null(this_output_code)) {
       name_vec_out <- paste0(name_vec, this_output_code)
    } else {
       name_vec_out <- NULL
    }
    if (!is.null(this_output_directory)) {
        if (is.null(name_vec_out)) {
            name_vec_out <- name_vec
        }
        name_vec_out <- paste(this_output_directory, name_vec_out, sep = "\\")
    }

    # print out file name
    print(file_name)

    # write out metafile
    if (!is.null(name_vec_out)) {
        write(paste(name_vec_spec, name_vec_out), file = file_name)
    } else {
        write(name_vec_spec, file = file_name)
    }

}
