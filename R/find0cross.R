find0cross <- function(x) {
  # x must be numeric
  stopifnot(is.numeric(x))

  # Length 0 and 1 can't have zero crossings
  if (length(x) < 2) {
    return(rep(FALSE, length(x)))
  }

  # Treat NAs like zeros
  x[is.na(x)] <- 0

  # Start looking at the first non-zero value
  if (all(x == 0)) {
    return(rep(FALSE, length(x)))
  }
  result <- logical(length(x))
  first_non0 <- which(x != 0)[1]
  result[1:first_non0] <- FALSE
  if (first_non0 == length(x)) {
    return(result)
  }
  s <- sign(x[first_non0])
  for (i in (first_non0 + 1):length(x)) {
    si <- sign(x[i])
    if (si == 0) {
      result[i] <- FALSE
    } else {
      result[i] <- s * si < 0
      s <- si
    }
  }
  result
}
