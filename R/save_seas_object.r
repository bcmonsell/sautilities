#' Save seas objects
#'
#' Stores \code{seas} command to reproduce the \code{seas} object \code{seas_obj} 
#' into the file \code{file_name.r}.
#'
#' Version 9.5, 5/14/2024
#'
#' @param seas_obj seasonal object. This is a required entry.
#' @param file_name character string; file name where \code{seas} object is stored; 
#'        default is the name of the seasonal object.
#' @param series_name character string; name of time series object used by the \code{seas} object; 
#'        default is the name of the seasonal object.
#' @param data_list character string; name of the list object that holds data; there is no default.
#' @param list_element character string; name of the list element used as data; 
#'        default is the name of the seasonal object.
#' @param user_reg character string; name of a time series matrix containing user defined 
#'        regressors; there is no default. 
#'        If not set, will set variables related to user defined regressors to \code{NULL} 
#'        in the static version of the \code{seas} object.
#' @param this_window logical indicator variable; determines if a span of the original series 
#         will be used in the analysis using the \code{window()} function.
#'        If \code{FALSE}, the entire series will be used in the saved file.
#' @param this_directory character string; optional directory where the spec file is stored.
#' @param this_sep character string; separator between elements of the file name. 
#'        Default is \code{"_"}.
#' @param print_out logical indicator variable; determines if an \code{out()} function is 
#'        printed at the end of the script. If \code{FALSE}, the \code{out()} function is commented out.
#'        Default is \code{FALSE}.
#' @return stores the \code{seas} command to reproduce the \code{seas} object 
#'         \code{seas_obj} into the file \code{file_name.r} - 
#'         if \code{file_name} is not specified, the name of the seasonal object will be used 
#'         to form the output file name.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' ukgas_seas <-
#'    seasonal::seas(UKgas, series.period = 4, arima.model = "(0 1 1)(0 1 1)",
#'                   x11 = "", transform.function = "log", forecast.maxlead = 20,
#'                   check.print = c( "pacf", "pacfplot" ))
#' \dontrun{save_seas_object(ukgas_seas, file_name = "ukgas_seas", series_name = "ukgas",
#'                  print_out = TRUE)}
#' @export
save_seas_object <- function(seas_obj = NULL, file_name = NULL, series_name = NULL, 
    data_list = NULL, list_element = NULL, user_reg = NULL, this_window = FALSE, 
    this_directory = NULL, this_sep = "_", print_out = FALSE) {
    # Author: Brian C. Monsell (OEUS) Version 9.5, 5/14/2024

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
    m_static <- invisible(seasonal::static(seas_obj, test = FALSE))

    # set up filename to be used to save seasonal object, with directory information if provided
    if (is.null(file_name)) {
        this_object_name <- deparse(substitute(seas_obj))
        file_name <- paste(this_object_name, ".r", sep = "")
    } else {
        this_object_name <- file_name
        file_name <- paste(file_name, ".r", sep = "")
    }
    if (!is.null(this_directory)) {
        file_name <- paste(this_directory, file_name, sep = "/")
    }

    this_x <- m_static$x
    line1 <- NULL
    if (!is.null(data_list)) {
        if (is.null(list_element)) {
            line1 <- paste0(data_list, "$", this_object_name)
        } else {
            line1 <- paste0(data_list, "$", list_element)
        }
    } else {
        if (length(this_x) == 3) {
            m_static$x[[3]] <- list_element
        } else {
            if (!is.null(series_name)) {
                if (this_x != series_name) {
                    line1 <- series_name
                }
            } else {
                if (m_static$x == "x") {
                    line1 <- paste(deparse(substitute(seas_obj)), "$x", sep = "")
                }
            }
        }
    }
    
    if (this_window) {
        this_start <- start(x)
        this_start_string <-  paste0("c(",this_start[1],",",this_start[2],")")
        this_end   <- end(x)
        this_end_string <-  paste0("c(",this_end[1],",",this_end[2],")")
        line1 <- paste0("window(", line1, ", start = ", this_start_string, 
                        ", end = ", this_end_string, ")")
    }

    line1 <- paste0(this_x, " <- ",line1)
    
    if (!is.null(user_reg)) {
        line1r <- paste0("xreg <- ", user_reg)
    } else {
        line1r <- NULL
        m_static$xreg <- NULL
        m_static$usertype <- NULL
    }
    
    # \code{line2} sets up call to \code{seas}
    line2 <- paste(this_object_name, this_sep, "seas <- ", sep = "")

    # \code{line3} sets up \code{out} statement, if requested
    if (print_out) {
        line3 <- paste("seasonal::out(", this_object_name, this_sep, "seas)", sep = "")
    } else {
        line3 <- paste("#seasonal::out(", this_object_name, this_sep, "seas)", sep = "")
    }

    # use \code{sink} to store \code{seas} output to file
    sink(file_name)
    if (!is.null(line1)) {
        cat(line1, fill = TRUE)
    }
    if (!is.null(line1r)) {
        cat(line1r, fill = TRUE)
    }
    cat(line2, fill = TRUE)
    cat("  seasonal::", fill = FALSE)
    print(m_static)
    cat(line3, fill = TRUE)
    sink()

}
