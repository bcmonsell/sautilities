#' Save List of Seas Objects
#'
#' Stores each element of a list of \code{seas} object \code{this_seas_object} 
#' into separate files
#'
#' Version 6.5, 9/26/2024
#'
#' @param this_seas_object_list list of \code{seasonal} objects.
#'        This is a required entry.
#' @param this_name_vec vector of character string; vector of series names from the list of 
#'        \code{seas} objects that will be saved.
#'        Default is all elements of the \code{seasonal} object list \code{this_seas_object_list} are saved.
#' @param this_directory character string; optional directory where the spec file is stored.
#' @param this_data_directory character string; optional directory where the data files are stored.
#'        Data files are assumed to have the same names as in \code{this_name_vec} with the file 
#'        extension specified in \code{this_ext}.
#'        Default is no change in \code{file} entry in the spec file.
#' @param this_ext character string; file extension for data files. Default is \code{".dat"}.
#' @param this_title_list list of character strings with the titles for each series.
#'        Default is to set \code{title} to the series name.
#' @param this_title_base character string; optional base for custom title; series name will be 
#'        added at the end of the title; quotes will be added by the routine.
#'        Default is to set \code{title} to the series name.
#' @param this_xreg_list list of character strings with the filenames of user defined regressors 
#'        or \code{NULL} for each series.
#'        Default is to not set \code{regression.file} for the individual series.
#' @param this_user_list list of vectors of character strings with the names of user defined 
#'        regressors or NULL for each series.
#'        Default is to not set \code{regression.file} for the individual series.
#' @param make_metafile logical scalar; if \code{TRUE}, generate a makefile for this set of files; 
#'        do not otherwise. Default is \code{FALSE}.
#' @param this_metafile_name - character string; base name of metafile to be generated. 
#'        If not specified, use name of list input as metafile name.
#'        Note - do not specify the \code{".mta"} file extension.
#' @param this_meta_directory - optional directory where the meta file is stored. 
#'        If not specified, the metafile will be saved in the current working directory.
#' @param this_output_directory - optional directory where the output files are stored in the 
#'        metafile. If not specified, the output files are saved in the current working directory.
#' @param include_directory - logical scalar; if \code{TRUE}, include directory specified in 
#'        \code{this_directory} with file name output.
#'        Otherwise, output only names in \code{this_name_vec}. Default is \code{FALSE}.
#' @return Stores the spec file representation of each element of a list of \code{seas} objects 
#'         into separate files.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' unemp_seas_list <-
#'    seasonal::seas(unemployment_list, slidingspans = "",
#'                   arima.model = "(0 1 1)(0 1 1)",
#'                   transform.function = "log",
#'                   regression.aictest = NULL, 
#'                   forecast.maxlead=36,
#'                   check.print = c( "pacf", "pacfplot" ))
#' test_seas_update <- 
#'      Filter(function(x) inherits(x, "seas"), unemp_seas_list)
#' \dontrun{save_spec_file_vec(test_seas_update, 
#'                    this_name_vec = c('n3000013', 'n3000014', 'n3000025', 'n3000026'),
#'                    this_data_directory = 'X:\\seasonalAdj\\testing\\data',
#'                    this_directory = 'X:\\seasonalAdj\\testing\\sautilities',
#'                    this_meta_directory = 'X:\\seasonalAdj\\testing\\sautilities',
#'                    this_title_base = 'Production Run for US Unemployment : ',
#'                    make_metafile = TRUE, include_directory = TRUE)}
#' @export
save_spec_file_vec <- 
    function(this_seas_object_list = NULL, this_name_vec = NULL,
             this_directory = NULL, this_data_directory = NULL, this_ext = ".dat",
             this_title_list = NULL, this_title_base = NULL, this_xreg_list = NULL, 
             this_user_list = NULL, make_metafile = FALSE, this_metafile_name = NULL,
             this_meta_directory = NULL, this_output_directory = NULL, 
             include_directory = FALSE) {
    # Author: Brian C. Monsell (OEUS) Version 6.5, 9/26/2024

    # check if a value is specified for \code{this_seas_object_list}
    if (is.null(this_seas_object_list)) {
        stop("must specify a list of seasonal objects")
    } else {
        if (!is.list(this_seas_object_list)) {
            stop("must specify a list")
        }
    }

    # filter elements of the list that are not seas objects
    seas_obj_list_update <-
        Filter(function(x) inherits(x, "seas"), this_seas_object_list)
  
    # Check if a value is specified for \code{this_name_vec}. 
    # If not specified, save all the elements.
    if (is.null(this_name_vec)) {
        this_name_vec <- names(seas_obj_list_update)
    }

    # for each name on the list, check to see if the name is an element in the list
    for (i in 1:length(this_name_vec)) {
        this_name <- this_name_vec[i]
        if (member_of_list(seas_obj_list_update, this_name)) {
            if (is.null(this_data_directory)) {
                this_file_name <- NULL
            } else {
                this_file_name <- paste0(this_data_directory, "\\", this_name, this_ext)
            }
            if (is.null(this_title_list)) {
                if (is.null(this_title_base)) {
                    this_title <- this_name
                } else {
                    this_title <- paste0(this_title_base, this_name)
                }
            } else {
                this_title <- this_title_list[[this_name]]
                if (is.null(this_title)) {
                    this_title <- this_name
                }
            }
            if (!is.null(this_xreg_list)) {
                this_xreg <- this_xreg_list[[this_name]]
                if (is.null(this_user_list)) {
                  this_user = NULL
                } else {
                  this_user <- this_user_list[[this_name]]
                }
                save_spec_file(seas_obj_list_update[[this_name]], this_name, this_directory,
                               data_file_name = this_file_name, this_title = this_title,
                               xreg_file_name = this_xreg, this_user_name = this_user)
            } else {
                save_spec_file(seas_obj_list_update[[this_name]], this_name, this_directory,
                           data_file_name = this_file_name, this_title = this_title)
            }
        } else {
            warning(paste0(this_name, "is not an element in the list"))
        }
    }

    if (make_metafile) {
        if (is.null(this_metafile_name)) {
            this_metafile_name <- deparse(substitute(seas_obj_list_update))
        }

        # write out metafile
        this_metafile_name <-
           save_metafile(this_seas_list = seas_obj_list_update,
                      this_name_vec = this_name_vec,
                      metafile_name = this_metafile_name,
                      this_directory = this_meta_directory, 
                      this_spec_directory = this_directory,
                      this_output_directory = this_output_directory,
                      include_directory = include_directory)
    }

}
