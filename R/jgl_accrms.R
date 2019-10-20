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
#' prh_ocdr <- jgl_ocdr(jgl_desc(prh_expl))
#' acc_filt <- jgl_filtacc(Araw_expl, 10, 90)
#' jgl_accrms(prh_ocdr, acc_filt)
#'
#' @export
jgl_accrms <- function(prh, A) {
  if (is.null(attr(A, "filtered"))) {
    stop("A is not filtered. Did you run `jgl_filtacc`?")
  }
  if (!"ocdr" %in% colnames(prh)) {
    stop("No `ocdr` column found. Did you run `jgl_ocdr`?")
  }
  if (is.null(attr(prh, "binwidth"))) {
    stop ("No binwidth found. Did you run `jgl_ocdr`?")
  }

  fs <- attr(prh, "fs")
  Afs <- attr(A, "Afs")
  if ((Afs / fs) %% 1 != 0) {
    stop("Ratio of `Afs` to `fs` must be an integer, not %.2f.", Afs / fs)
  }
  if (nrow(A) / (Afs / fs) != nrow(prh)) {
    stop(sprintf("`A` must have exactly %i times as many rows as `prh`.",
                 Afs / fs))
  }

  # Create a [nrow(prh) x binsize] matrix for each axis of acceleration and
  # apply rms to each column
  binsize <- attr(prh, "binwidth") * fs
  rms_mtx <- matrix(0, nrow = nrow(prh), ncol = binsize)
  idx_to_A <- function(idx) {
    ((idx[1] - 1) * Afs / fs + 1):(idx[length(idx)] * Afs / fs)
  }
  calc_rms <- function(col) {
    acc_mat <- sapply(
      seq(nrow(prh)),
      function(i) {
        bin_start <- max(1, floor(i - binsize / 2))
        bin_end <- min(nrow(prh), floor(i + binsize / 2))
        result <- A$A[idx_to_A(bin_start:bin_end), col]
        result_size <- (binsize + 1) * Afs / fs
        if (length(result) < result_size) {
          if (bin_start == 1) {
            result <- c(rep(NA, result_size - length(result)), result)
          } else {
            result <- c(result, rep(NA, result_size - length(result)))
          }
        }
        result
      }
    )
    apply(acc_mat, 2, function(x) 20 * log10(rms(x)))
  }
  rms_mat <- sapply(1:3, calc_rms)

  result <- prh
  result$rms_acc <- rms_mat
  result
}
