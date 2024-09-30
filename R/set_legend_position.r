#' generate position of plot legend
#'
#' Generate position code for the \code{legend} command based on the series being plotted.
#'
#' Version 2.8, 5/25/2023
#'
#' @param data_matrix numeric matrix; matrix where all series being plotted are stored as columns.
#'        This is a required entry.
#' @param this_plot_start Integer scalar; start date of the plot.  
#'        This is a required entry.
#' @param this_plot_freq Integer scalar; Frequency of time series plotted. Default is 12. 
#' @param time_disp Integer scalar; number of observations on the x-axis taken up by the legend. 
#'        Default is 3. 
#' @param value_disp Numeric scalar; factor representing the percentage of the y axis taken up 
#'         by the legend. Default is 1/6. 
#' @param default_code Character string; default position code if the corners are not available. 
#'         Default is \code{"top"}. Possible values are \code{"bottomright"}, \code{"bottom"}, 
#'         \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"topright"}, 
#'         \code{"top"}, \code{"right"} and \code{"center"}.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @return Position codes for the legend command. Possible values are \code{"bottomright"}, 
#'         \code{"bottom"}, \code{"bottomleft"}, \code{"topleft"}, \code{"topright"} 
#'         and the value of \code{default_code}.
#' @examples
#' shoes_seas <- 
#'     seasonal::seas(shoes2007, slidingspans = "", transform.function = "log", x11 = "",
#'                    forecast.maxlead=36, check.print = c( "pacf", "pacfplot" ))
#' this_series <- shoes2007
#' this_sa     <- seasonal::final(shoes_seas)
#' this_legend_position <- 
#'     set_legend_position(cbind(this_series, this_sa), start(this_series),
#'                         this_plot_freq = 4, time_disp = 8, value_disp = 1/8,
#'                         default_code = "top")                      
#' @import stats
#' @import graphics
#' @export
set_legend_position <- function(data_matrix = NULL, this_plot_start = NULL,
                                this_plot_freq = 12, time_disp = 3, value_disp = 1/6,
                                default_code = "top") {
    # Author: Brian C. Monsell (OEUS) Version 2.8, 5/25/2023
    if (is.null(data_matrix)) {
        stop("Argument data_matrix must be specified.")
    }
    if (is.null(this_plot_start)) {
        stop("Argument this_plot_start must be specified.")
    }
    
    temp_matrix <- ts(data_matrix, start = this_plot_start, 
                      frequency = this_plot_freq)
    time_temp <- time(temp_matrix)
    
    start_temp <- window(temp_matrix, end = time_temp[time_disp])
    this_y_limit <- par("usr")[3:4]
    top_legend_limit <- 
        this_y_limit[2] - (this_y_limit[2] - this_y_limit[1]) * value_disp
    
    end_temp <- window(temp_matrix, start = time_temp[length(time_temp) - time_disp])
    bottom_legend_limit <- 
        this_y_limit[1] + (this_y_limit[2] - this_y_limit[1]) * value_disp
    
    if (sum(start_temp > top_legend_limit) == 0) {
        return("topleft")
    }
    
    if (sum(end_temp > top_legend_limit) == 0) {
        return("topright")
    }
    
    if (sum(end_temp < bottom_legend_limit) == 0) {
        return("bottomright")
    }
    
    if (sum(start_temp < bottom_legend_limit) == 0)  {
        return("bottomleft")
    }

    return(default_code)
    
}
    
