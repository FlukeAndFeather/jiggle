#' Orientation-corrected depth rate
#'
#' Uses steep descent phases to estimate known speed values.
#'
#' @param prh PRH data frame with \code{desc_id} column (see
#'   \code{\link{jgl_desc}})
#' @param binwidth Size of bin to calculate OCDR (in seconds)
#'
#' @return PRH data frame with \code{ocdr} column and an attribute
#'   \code{binwidth}.
#'
#' @examples
#' prh_expl %>%
#'   jgl_desc() %>%
#'   get_ocdr()
#'
#' @export
get_ocdr <- function(prh, binwidth = 1) {
  if (!"desc_id" %in% colnames(prh)) {
    stop("Column `desc_id` not found. Did you run `jgl_desc`?")
  }
  fs <- attr(prh, "fs")
  binsize <- binwidth * fs

  # Get OCDR of one descent
  get_ocdr_desc <- function(depth, pitch, desc_id) {
    if (is.na(desc_id)){
      return(rep(NA, length(depth)))
    }
    max_depth <- RcppRoll::roll_max(depth,
                                    binsize,
                                    fill = NA,
                                    align = "center")
    min_depth <- RcppRoll::roll_min(depth,
                                    binsize,
                                    fill = NA,
                                    align = "center")
    delta_depth <- max_depth - min_depth
    mean_pitch <- RcppRoll::roll_mean(-pitch,
                                      binsize,
                                      fill = NA,
                                      align = "center")
    distance <- delta_depth / sin(mean_pitch)
    ocdr_desc <- distance / binwidth
    ocdr_desc
  }
  result <- suppressWarnings(
    prh %>%
      dplyr::group_by(.data$desc_id) %>%
      dplyr::mutate(ocdr = get_ocdr_desc(.data$depth,
                                         .data$pitch,
                                         .data$desc_id[1])) %>%
      dplyr::ungroup()
  )
  attr(result, "fs") <- attr(prh, "fs")
  attr(result, "binwidth") <- binwidth
  result
}
