findpeaks <- function(x, width, thr) {
  local_max <- RcppRoll::roll_max(x, n = width, fill = NA)
  i_peak <- seq_along(x)[x == local_max & x >= thr]
  x_peak <- x[i_peak]
  data.frame(i = i_peak,
             x = x_peak)
}
