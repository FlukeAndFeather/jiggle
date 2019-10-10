#' Root mean square of acceleration
#'
#' Calculate root mean square (RMS) of acceleration in bins corresponding to
#' OCDR bins.
#'
#' @param prh PRH data frame with \code{ocdr} column and \code{binwidth}
#'   attribute.
#' @param A Acceleration data frame. Columns \code{Atime} (POSIXct) and
#'   \code{A} (3-column matrix) with an attribute, \code{Afs}, for the sampling
#'   frequency.
#'
#' @return PRH data frame with \code{rms_acc} matrix column.
#'
#' @examples
#' prh_ocdr <- get_ocdr(find_desc(prh_expl))
#' acc_filt <- filter_acc(Araw_expl, 10, 90)
#' get_rms_acc(prh_ocdr, acc_filt)
#'
#' @export
get_rms_acc <- function(prh, A) {
  if (is.null(attr(A, "filtered"))) {
    stop("A is not filtered. Did you run `filter_acc`?")
  }
  if (!"ocdr" %in% colnames(prh)) {
    stop("No `ocdr` column found. Did you run `get_ocdr`?")
  }
  if (is.null(attr(prh, "binwidth"))) {
    stop ("No binwidth found. Did you run `get_ocdr`?")
  }

  fs <- attr(prh, "fs")
  Afs <- attr(A, "Afs")
  binsize <- attr(prh, "bindwidth") * fs
  idx_to_A <- function(idx) {
    ((idx[1] - 1) * Afs / fs + 1):(idx[length(idx)] * Afs / fs)
  }
  rms_mtx <- matrix(NA, nrow = nrow(prh), ncol = 3)
  for (i in 1:nrow(rms_mtx)) {
    if (!is.na(prh$ocdr[i])) {
      bin_start <- max(1, floor(i - binsize / 2))
      bin_end <- min(nrow(rms_mtx), floor(i + binsize / 2))
      acc <- A$A[idx_to_A(bin_start:bin_end), ]
      rms_mtx[i, ] <- apply(acc, 2, rms)
    }
  }

  result <- prh
  result$rms_acc <- rms_mtx
  result
}
