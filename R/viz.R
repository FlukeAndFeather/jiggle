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
    ggplot2::scale_y_continuous("Pitch (deg)",
                                breaks = pitch_breaks,
                                labels = as_degrees) +
    ggplot2::expand_limits(y = pitch_breaks) +
    ggplot2::theme_minimal()
  cowplot::plot_grid(depth_plot, pitch_plot,
                     align = "v",
                     ncol = 1,
                     rel_heights = c(2, 3))
}

#' Plot OCDR
#'
#' Create a plot of orientation-corrected depth rate during descents.
#'
#' @param prh PRH data frame with OCDR calculated
#'
#' @return A gg object (see \code{cowplot})
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
  depth_plot <- plot_depth(prh)
  ocdr_plot <- ggplot2::ggplot(prh, ggplot2::aes(time, ocdr)) +
    ggplot2::geom_line(size = 0.2) +
    ggplot2::labs(y = "OCDR (m/s)") +
    ggplot2::expand_limits(y = 0) +
    ggplot2::theme_minimal()
  cowplot::plot_grid(depth_plot, ocdr_plot,
                     align = "v",
                     ncol = 1,
                     rel_heights = c(2, 3))
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
  speed_plot <- ggplot2::ggplot(prh, ggplot2::aes(time, smooth_speed)) +
    ggplot2::geom_line(size = 0.2) +
    ggplot2::scale_y_continuous("Speed (m/s)") +
    ggplot2::expand_limits(y = 0) +
    ggplot2::theme_minimal()
  cowplot::plot_grid(depth_plot, speed_plot,
                     align = "v",
                     ncol = 1,
                     rel_heights = c(2, 3))
}

#' Plot filtered acceleration
#'
#' Plots the filtered acceleration signal with depth profile for context. Note,
#' raw acceleration is extremely large so this function is best used with
#' smaller subsets.
#'
#' @param acc Filtered acceleration data frame (see \code{\link{jgl_filtacc}}).
#' @param prh PRH data frame.
#'
#' @return A gg object (see \code{cowplot})
#'
#' @examples
#' plot_filt(jgl_filtacc(Araw_expl), prh_expl)
#'
#' @export
plot_filt <- function(acc, prh) {
  if (is.null(attr(acc, "filtered"))) {
    stop("`acc` must be filtered. Did you run `jgl_filtacc`?")
  }
  depth_plot <- plot_depth(prh, show_desc = FALSE)
  acc_plot <- data.frame(time = acc$time,
                         Ax = acc$A[, 1],
                         Ay = acc$A[, 2],
                         Az = acc$A[, 3]) %>%
    tidyr::gather(axis, acc, Ax:Az) %>%
    ggplot2::ggplot(ggplot2::aes(time, acc)) +
    ggplot2::geom_line(size = 0.2) +
    ggplot2::facet_grid(rows = ggplot2::vars(axis)) +
    ggplot2::labs(x = "Time",
                  y = "Acceleration (g)") +
    ggplot2::theme_minimal()
  cowplot::plot_grid(depth_plot, acc_plot,
                     align = "v",
                     axis = "lr",
                     ncol = 1,
                     rel_heights = c(2, 3))
}

#' Plot OCDR ~ jiggle
#'
#' Plots OCDR against the norm of the triaxial jiggle.
#'
#' @param prh PRH data frame with `ocdr` and `rms_acc` columns.
#'
#' @return A ggplot object
#'
#' @examples
#' prh_ocdr <- jgl_ocdr(jgl_desc(prh_expl))
#' acc_filt <- jgl_filtacc(Araw_expl, 10, 90)
#' acc_rms <- jgl_accrms(prh_ocdr, acc_filt)
#' plot_rms(acc_rms)
#'
#' @export
plot_rms <- function(prh) {
  if (!"ocdr" %in% colnames(prh)) {
    stop("Column `ocdr` not found. Did you run `jgl_ocdr`?")
  }
  if (!"rms_acc" %in% colnames(prh)) {
    stop("Column `rms_acc` not found. Did you run `jgl_accrms`?")
  }
  prh %>%
    dplyr::mutate(
      jgl_norm = apply(rms_acc, 1, function(row) sum(row))
    ) %>%
    ggplot2::ggplot(ggplot2::aes(jgl_norm, ocdr)) +
    ggplot2::geom_point(size = 0.2) +
    ggplot2::labs(x = expression("Jiggle RMS amplitude (dB re. 1 m" ^ 2 * " s" ^ -4 * ")"),
                  y = "OCDR (m/s)") +
    ggplot2::theme_minimal()
}

#' Depth plot utility function
#'
#' @param prh PRH data frame
#' @param hide_x If TRUE (default), hide x-axis title and text.
#' @param show_desc If TRUE (default), highlight descent periods.
#'
#' @noRd
plot_depth <- function(prh, hide_x = TRUE, show_desc = TRUE) {
  depth_plot <- ggplot2::ggplot(prh, ggplot2::aes(time, depth)) +
    ggplot2::geom_line(size = 0.2) +
    ggplot2::scale_y_reverse() +
    ggplot2::labs(y = "Depth (m)") +
    ggplot2::theme_minimal()
  if (show_desc) {
    depth_plot <- depth_plot +
      ggplot2::geom_line(ggplot2::aes(group = desc_id),
                         data = tidyr::drop_na(prh, desc_id),
                         color = "blue",
                         size = 1)
  }
  if (hide_x) {
    depth_plot <- depth_plot +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank())
  }
  depth_plot
}
