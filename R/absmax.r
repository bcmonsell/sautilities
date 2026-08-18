#' Maximum absolute value of a vector
#'
#' Generates the maximum of the absolute value of a numeric vector.
#'
#' Version 2.1, 2026-06-04
#'
#' @param x Vector of numbers. This is a required element.
#' @param return_abs Logical scalar; if \code{TRUE}, return the absolute maximum value.
#'        If \code{FALSE}, maintain the sign of the original value.
#'        Default is \code{TRUE}.
#' @return Maximum of the absolute value of a vector.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' r50 <- rnorm(50)
#' r50.absmax <- absmax(r50)
#' @export
absmax <- function(x = NULL, return_abs = TRUE) {
  # Author: Brian C. Monsell (OEUS) Version 2.1, 2026-06-04
  if (is.null(x)) {
    cat("must specify a numeric vector")
    return(NULL)
  }
  this_pos <- which.max(abs(x))
  if (return_abs) {
    return(abs(x)[this_pos])
  } else {
    return(x[this_pos])
  }
}
