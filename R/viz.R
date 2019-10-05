#' Visualize descent phases within dive profile
viz_desc <- function(desc) {
  ggplot2::ggplot(desc, ggplot2::aes())
}

#' Visualize constant pitch periods in descents
viz_const_pitch <- function(const_pitch) {
  ggplot2::ggplot(const_pitch, ggplot2::aes(i, pitch)) +
    ggplot2::geom_line() +
    ggplot2::geom_line(ggplot2::aes(group = flat),
                       tidyr::drop_na(const_pitch, flat),
                       color = "red") +
    ggplot2::scale_y_continuous("Pitch (deg)",
                                labels = as_degrees) +
    ggplot2::expand_limits(y = -pi / 2) +
    ggplot2::facet_wrap(~ desc,
                        scales = "free_x") +
    ggplot2::theme_minimal()
}
