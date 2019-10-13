#' Plot descent phases within dive profile
#'
#' Highlights descent phases used to calculate OCDR. Requires package
#' `cowplot`.
#'
#' @param prh PRH data frame with `desc_id` column
#'
#' @return A gg object (see \code{cowplot})
#'
#' @examples
#' plot_desc(jgl_desc(prh_expl))
#'
#' @export
plot_desc <- function(prh) {
  if (!"desc_id" %in% colnames(prh)) {
    stop("Column `desc_id` not found. Did you run `jgl_desc`?")
  }
  if (!requireNamespace("cowplot", quietly = TRUE)) {
    stop("Package `cowplot` is required for plot_desc")
  }
  depth_plot <- plot_depth(prh)
  pitch_breaks <- seq(-pi / 2, pi / 2, length.out = 7)
  pitch_plot <- ggplot2::ggplot(prh, ggplot2::aes(time, pitch)) +
    ggplot2::geom_line(size = 0.2) +
    ggplot2::geom_line(ggplot2::aes(group = desc_id),
                       data = tidyr::drop_na(prh, desc_id),
                       color = "blue",
                       size = 1) +
    ggplot2::scale_y_continuous(breaks = pitch_breaks,
                                labels = as_degrees) +
    ggplot2::expand_limits(y = pitch_breaks) +
    ggplot2::theme_minimal()
  cowplot::plot_grid(depth_plot, pitch_plot,
                     align = "h",
                     ncol = 1,
                     rel_heights = c(2, 3))
}

#' Plot OCDR
#'
#' Create a plot of orientation-corrected depth rate during descents.
#'
#' @param prh PRH data frame with OCDR calculated
#'
#' @return A ggplot object
#'
#' @examples
#' prh_expl %>%
#'   jgl_desc() %>%
#'   jgl_ocdr() %>%
#'   plot_ocdr()
#'
#' @export
plot_ocdr <- function(prh) {
  if (!"ocdr" %in% colnames(prh)) {
    stop("Column `ocdr` not found. Did you run `jgl_ocdr`?")
  }
  if (!requireNamespace("cowplot", quietly = TRUE)) {
    stop("Package `cowplot` is required for plot_ocdr")
  }
  ocdr_plot <- ggplot2::ggplot(prh, ggplot2::aes(time, depth)) +
    ggplot2::geom_line(size = 0.2) +
    ggplot2::geom_point(ggplot2::aes(color = ocdr),
                        data = tidyr::drop_na(prh, ocdr)) +
    ggplot2::scale_y_reverse() +
    ggplot2::scale_color_gradient("OCDR",
                                  low = "#F5AF19",
                                  high = "#F12711") +
    ggplot2::expand_limits(color = 0) +
    ggplot2::theme_minimal()
  ocdr_plot
}

#' Plot speed
#'
#' Plot speed as estimated from tag jiggle. Requires package `cowplot`.
#'
#' @param prh PRH data frame with `speed` column
#' @param smooth Width of smoothing window in seconds
#'
#' @return A gg object (see \code{cowplot})
#'
#' @examples
#' acc_filt <- jgl_filtacc(Araw_expl, 10, 90)
#' prh_expl %>%
#'   jgl_desc() %>%
#'   jgl_ocdr() %>%
#'   jgl_accrms(acc_filt) %>%
#'   jgl_fit() %>%
#'   plot_speed()
#'
#' @export
plot_speed <- function(prh, smooth = 1) {
  if (!"speed" %in% colnames(prh)) {
    stop("Column `speed` not found. Did you run `jgl_fit`?")
  }
  if (!requireNamespace("cowplot", quietly = TRUE)) {
    stop("Package `cowplot` is required for plot_desc")
  }
  depth_plot <- plot_depth(prh)
  prh$smooth_speed <- RcppRoll::roll_mean(prh$speed,
                                          smooth * attr(prh, "fs"),
                                          fill = NA)
  prh$true_speed <- RcppRoll::roll_mean(speeds,
                                        smooth * attr(prh, "fs"),
                                        fill = NA)
  speed_plot <- ggplot2::ggplot(prh, ggplot2::aes(time, smooth_speed)) +
    ggplot2::geom_line(size = 0.2) +
    ggplot2::geom_line(ggplot2::aes(y = true_speed),
                       linetype = "dashed",
                       color = "red") +
    ggplot2::scale_y_continuous("speed") +
    ggplot2::theme_minimal()
  cowplot::plot_grid(depth_plot, speed_plot,
                     align = "h",
                     ncol = 1,
                     rel_heights = c(2, 3))
}

#' Depth plot utility function
#'
#' @param prh PRH data frame
#' @param hide_x If TRUE (default), hide x-axis title and text.
#'
#' @noRd
plot_depth <- function(prh, hide_x = TRUE) {
  depth_plot <- ggplot2::ggplot(prh, ggplot2::aes(time, depth)) +
    ggplot2::geom_line(size = 0.2) +
    ggplot2::geom_line(ggplot2::aes(group = desc_id),
                       data = tidyr::drop_na(prh, desc_id),
                       color = "blue",
                       size = 1) +
    ggplot2::scale_y_reverse()
  if (hide_x) {
    depth_plot +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank())
  } else {
    depth_plot
  }
}
