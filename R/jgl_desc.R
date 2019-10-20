#' Find steep descent phases at the beginning of dives
#'
#' Annotates a PRH data frame with descent phases. Each descent phase begins
#' with the start of a dive and ends at the beginning of the bottom phase. Only
#' the part of the descent exceeding the pitch threshold is annotated.
#'
#' @param prh A PRH data frame
#' @param depth_thr Minimum depth of a dive
#' @param pitch_thr Minimum pitch to count as steep descent (in radians)
#' @param dur_thr Minimum duration of steep descent (in seconds)
#'
#' @return A PRH data frame with a column `desc_id` identifying each descent.
#'
#' @examples
#' jgl_desc(prh_expl)
#'
#' @export
jgl_desc <- function(prh,
                     depth_thr = 5,
                     pitch_thr = -45 * pi / 180,
                     dur_thr = 5) {
  depth <- prh$depth
  pitch <- prh$pitch
  fs <- attr(prh, "fs")

  # Split depth profile into dives
  # Some dives may be invalid if animal briefly exceeds depth threshold during
  # surface interval
  dive_start <- depth >= depth_thr & lag0(depth) < depth_thr
  dive_id <- cumsum(dive_start)
  dive_id[depth < depth_thr] <- NA

  # Isolate each dive's descent phase
  depth_peaks <- findpeaks(depth, width = fs * 10, thr = depth_thr)$i
  desc_end <- purrr::map_dbl(
    which(dive_start),
    ~ depth_peaks[depth_peaks > .x][1]
  )

  # Remove false descents
  # If multiple descent starts are associated with a single descent end, then
  # only the last descent start is valid
  desc_phases <- data.frame(
    begin = which(dive_start),
    end = desc_end
  ) %>%
    dplyr::group_by(end) %>%
    dplyr::filter(begin == max(begin)) %>%
    dplyr::ungroup() %>%
    # Apply first-pass duration threshold
    dplyr::filter((end - begin) / fs >= dur_thr)
  desc_phases$desc_id = seq(nrow(desc_phases))

  # Join descent phases to PRH
  desc_phases_long <- desc_phases %>%
    dplyr::mutate(time = purrr::map2(.data$begin,
                                     .data$end,
                                     ~ prh$time[.x:.y])) %>%
    tidyr::unnest(time) %>%
    dplyr::select(.data$time, .data$desc_id)
  result <- dplyr::left_join(prh, desc_phases_long, by = "time")
  attr(result, "fs") <- attr(prh, "fs")

  # Apply pitch threshold
  for (i in na.omit(unique(result$desc_id))) {
    descent <- dplyr::filter(result, desc_id == i)
    descent$desc_id <- NA
    # Look for longest sequence exceeding pitch and duration threshold
    pitch_rle <- rle(descent$pitch <= pitch_thr)
    if (any(pitch_rle$values)) {
      longest_pitch <- max(pitch_rle$lengths[pitch_rle$values])
      if (longest_pitch / fs > dur_thr) {
        which_longest <- which(
          (pitch_rle$lengths == longest_pitch) & pitch_rle$values
        )[1]
        if (which_longest == 1) {
          begin <- 1
        } else {
          begin <- sum(pitch_rle$lengths[1:(which_longest - 1)]) + 1
        }
        end <- begin + pitch_rle$lengths[which_longest] - 1
        descent$desc_id[begin:end] <- i
      }
    }

    result$desc_id[which(result$desc_id == i)] <- descent$desc_id
  }

  result
}
