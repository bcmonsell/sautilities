#' Save spec file representation of \code{seas} object
#'
#' Stores the spec file representation of the \code{seas} object \code{seas_obj} 
#' into the file \code{file_name.spc}.
#'
#' Version 4.3, 5/14/2024
#'
#' @param seas_obj \code{seasonal} object. This is a required entry.
#' @param file_name character string; file name where \code{seas} object is stored; 
#'        default is the name of the \code{seasonal} object
#' @param this_directory character string; optional directory where the spec file is stored.
#' @param this_data_directory character string; optional directory where the data files are stored.
#'        Default is no change in \code{file} entry in the spec file.
#' @param data_file_name character string; optional external file name where data file is stored.
#'        Path should be included with file name if data file is not in working directory; 
#'        quotes will be added by the routine. 
#'        Default is no change in \code{file} entry in the spec file.
#' @param xreg_file_name character string; optional external file name where user defined 
#'        regressors are stored. Path should be included with file name if data file is not in 
#'        working directory; quotes will be added by the routine.
#'        Default is no change in \code{file} entry in the spec file.
#' @param this_user_name vector of character strings; optional names for the user-defined  
#'        regressors. Should only appear if \code{xreg_file_name} is specified.
#' @param this_title character string; optional custom title; quotes will be added by the routine.
#'        Default is no change in \code{title} entry in the spec file.
#' @return stores the spec file representation of the \code{seas} object \code{seas_obj} 
#'         into the file \code{file_name.spc}
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' ukgas_seas <- seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)", 
#'                              x11="", transform.function = "log", forecast.maxlead=20,
#'                              check.print = c( "pacf", "pacfplot" ))
#' \dontrun{save_spec_file(ukgas_seas, 'ukgas', 
#'                        data_file_name = "ukgas.dat",
#'                        this_directory = "X:\\seasonalAdj\\testing\\sautilities",
#'                        this_title = "Production run for quarterly UK Gas")}
#' @export
save_spec_file <- function(seas_obj = NULL, file_name = NULL, this_directory = NULL,
                           this_data_directory = NULL, data_file_name = NULL,  
                           xreg_file_name = NULL, this_user_name = NULL, this_title = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 4.3, 5/14/2024

    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
           stop("First argument must be a seas object")
        }
    }

    # extract data from the seasonal object \code{seas_obj}
    x <- seas_obj$x

    # save static version of object
    # m_static <- seasonal::static(seas_obj, test = FALSE, x11.filter = TRUE)

    # set up filename to be used to save spec file, with directory information if provided
    if (is.null(file_name)) {
        this_object_name <- deparse(substitute(seas_obj))
        file_name <- paste(this_object_name, ".spc", sep = "")
    } else {
        file_name <- paste(file_name, ".spc", sep = "")
    }
    if (!is.null(this_directory)) {
        file_name <- paste(this_directory, file_name, sep = "/")
    }

    this_spc <- seasonal::spc(seas_obj)
    if (!is.null(this_title)) {
        this_spc$series$title <- paste0("\"", this_title, "\"")
    }
    if (!is.null(data_file_name)) {
        if (!is.null(this_data_directory)) {
            data_file_name <- paste0(this_data_directory, "\\", data_file_name)
        }
        this_spc$series$file <- paste0("\"", data_file_name, "\"")
    }
    if (!is.null(xreg_file_name)) {
        this_spc$regression$file <- paste0("\"", xreg_file_name, "\"")
    }
    if (!is.null(this_user_name)) {
        this_spc$regression$user <- this_user_name
    }

    # print out file name
    print(file_name)

    # use sink to store X-13 specs to file
    sink(file_name)
    print(this_spc)
    sink()

}
