find0cross <- function(x) {
  # x must be numeric
  stopifnot(is.numeric(x))

  # Length 0 and 1 can't have zero crossings
  if (length(x) < 2) {
    return(rep(FALSE, length(x)))
  }

  # Recursively find zero-crossing values
  is_cross <- function(y, s) {
    if (length(y) == 0) {
      return(logical(0))
    }
    s1 <- sign(y[1])
    yn <- y[-1]
    if (s1 == 0) {
      c(FALSE, is_cross(yn, s))
    } else {
      c(s1 * s < 0, is_cross(yn, s1))
    }
  }

  # Start looking at the first non-zero value
  if (all(x == 0)) {
    return(rep(FALSE, length(x)))
  }
  first_non0 <- which(x != 0)[1]
  c(rep(FALSE, first_non0), is_cross(x[-c(1:first_non0)], sign(x[first_non0])))
}
