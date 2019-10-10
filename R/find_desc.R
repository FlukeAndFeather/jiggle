#' Find steep descent phases at the beginning of dives
#'
#' Annotates a PRH data frame with descent phases. Each descent phase begins
#' with the start of a dive and ends at the beginning of the bottom phase. Only
#' the part of the descent exceeding the pitch threshold is annotated.
#'
#' @param prh A PRH data frame
#' @param depth_thr Minimum depth of a dive
#' @param pitch_thr Minimum pitch to count as steep descent (in radians)
#'
#' @export
find_desc <- function(prh, depth_thr = 5, pitch_thr = -45 * pi / 180) {
  depth <- prh$depth
  pitch <- prh$pitch

  # Split depth profile into dives
  desc_start <- depth >= depth_thr & lag0(depth) < depth_thr
  dive_id <- cumsum(desc_start)
  dive_id[depth < depth_thr] <- NA

  # Isolate each dive's descent phase
  depth_peaks <- findpeaks(depth, width = attr(prh, "fs") * 10, thr = 5)
  colnames(depth_peaks)[2] <- "depth"
  desc_end <- purrr::map_dbl(
    which(desc_start),
    ~ depth_peaks$i[depth_peaks$i > .x][1]
  )

  # Find steep regions in descent phases
  desc_phases <- data.frame(
    begin = seq_along(depth)[desc_start],
    end = desc_end
  ) %>%
    dplyr::mutate(desc_id = factor(seq(length(.data$begin)))) %>%
    dplyr::group_by(.data$desc_id) %>%
    dplyr::group_modify(
      function(row, key) {
        i <- row$begin:row$end
        time <- prh$time[i]
        data.frame(time, pitch = pitch[i])
      }
    ) %>%
    dplyr::ungroup()
  desc_phases <- desc_phases[desc_phases$pitch <= pitch_thr,
                             c("time", "desc_id")]

  result <- dplyr::left_join(prh, desc_phases, by = "time")
  attr(result, "fs") <- attr(prh, "fs")
  result
}
