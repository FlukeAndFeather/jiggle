#' Format radians as degrees
#'
#' Utility function for formatting radians as degrees, with control for number
#' of digits.
#'
#' @param rad Values in radians
#' @param digits Number of digits to display
#'
#' @return A character vector with degree equivalents of `rad`
#'
#' @noRd
as_degrees <- function(rad, digits = 0) {
  format(rad * 180 / pi, digits = digits)
}

#' Root mean square
#'
#' Calculate the root mean square of a vector
#'
#' @param x Numeric vector
#'
#' @return The root mean square of `x` (numeric scalar)
#'
#' @noRd
rms <- function(x) {
  if (!is.vector(x, mode = "numeric")) {
    stop("x must be a numeric vector.")
  }
  sqrt(sum((x - mean(x))^2))
}

#' Lag with default 0
#'
#' Wrapper for dplyr::lag, with `default` set to 0
#'
#' @param x Numeric vector
#'
#' @return `x` lagged `n` times, filled with 0s
#'
#' @noRd
lag0 <- function(x, n = 1) {
  dplyr::lag(x, default = 0)
}

#' Find peaks in a vector
#'
#' Identifies peaks as points where the vector intersects the maximum value in
#' a window centered on the point
#'
#' @param x Numeric vector
#' @param width Size of rolling window
#' @param thr Minimum value of x to count as a peak
#'
#' @return Data frame with columns i (index of peaks) and x (value at peak)
findpeaks <- function(x, width, thr) {
  local_max <- RcppRoll::roll_max(x, n = width, fill = NA)
  i_peak <- seq_along(x)[x == local_max & x >= thr]
  x_peak <- x[i_peak]
  data.frame(i = i_peak,
             x = x_peak)
}
