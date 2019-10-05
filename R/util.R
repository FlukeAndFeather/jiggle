#' Format radians as degrees
#'
#' Utility function for formatting radians as degrees, with control for number
#' of digits.
#'
#' @param rad Values in radians
#' @param digits Number of digits to display
#'
#' @return A character vector with degree equivalents of `rad`
as_degrees <- function(rad, digits = 0) {
  format(rad * 180 / pi, digits = digits)
}
