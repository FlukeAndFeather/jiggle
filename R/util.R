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

#' Band pass filter acceleration
#'
#' Filter the acceleration signal between low and high frequency thresholds.
#'
#' @param A Acceleration (3d matrix)
#' @param Afs Acceleration sampling frequency
#' @param low,high Low and high cutoff frequencies for band-pass filter
#'
#' @return Band-pass filtered acceleration in a matrix with the same dimensions
#'  as A
#'
#' @noRd
filter_acc <- function(A, Afs, low, high) {
  # A must be a 3d numeric matrix
  if (!is.matrix(A)) {
    stop("`A` must be a matrix")
  }
  if (!is.numeric(A)) {
    stop("`A` must be numeric")
  }
  if (ncol(A) != 3) {
    stop(sprintf("`A` must have 3 columns, not %d", ncol(A)))
  }

  # Afs, low, and high must all be scalar numbers
  # There has to be a way to avoid repeating this
  if (!is.numeric(Afs) || length(Afs) > 1) {
    stop("Afs must be a scalar")
  }
  if (!is.numeric(low) || length(low) > 1) {
    stop("low must be a scalar")
  }
  if (!is.numeric(high) || length(high) > 1) {
    stop("high must be a scalar")
  }

  # Can't filter above the Nyquist frequency
  nyquist <- Afs / 2
  if (high > nyquist) {
    stop(sprintf(
      "`high` (%.2f) must be less than the Nyquist frequency (Afs / 2 = %.2f)",
      high,
      nyquist
    ))
  }

  freq <- c(low, high) / nyquist
  h <- signal::fir1(127, freq, "pass")
  apply(A, 2, function(col) signal::filtfilt(h, col))
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
