#' Orientation-corrected depth rate
#'
#' Uses steep descent phases with constant pitch to generate known speed
#' values.
#'
#' @examples
#' const_pitch <- find_desc(prh$depth, prh$pitch, prh$fs) %>%
#'   tidyr::drop_na(desc) %>%
#'   dplyr::group_by(desc) %>%
#'   dplyr::group_modify(~ find_const_pitch(.x, .y,
#'                                         width = prh$fs,
#'                                         fs = prh$fs)) %>%
#'   dplyr::ungroup()
#' get_ocdr(const_pitch, prh$fs)
get_ocdr <- function(const_pitch, fs) {
  get_ocdr1 <- function(depth, pitch) {
    mean_pitch <- mean(-pitch)
    delta_depth <- max(depth) - min(depth)
    dist <- delta_depth / sin(mean_pitch)
    dur <- length(depth) / fs
    dist / dur
  }
  const_pitch %>%
    tidyr::drop_na(flat) %>%
    dplyr::group_by(desc, flat) %>%
    dplyr::summarize(ocdr = get_ocdr1(depth, pitch),
                     fstart = min(i),
                     fend = max(i))
}

#' Find steep descent phases at the beginning of dives
find_desc <- function(depth,
                      pitch,
                      fs,
                      depth_thr = 5,
                      pitch_thr = -45 * pi / 180) {
  # Split depth profile into dives
  dive_start <- depth >= depth_thr & dplyr::lag(depth, default = 0) < depth_thr
  dive_num <- cumsum(dive_start)
  dive_num[depth < depth_thr] <- NA

  # Isolate each dive's descent phase
  depth_peaks <- findpeaks(depth, width = fs * 10, thr = 5) %>%
    dplyr::rename(depth = x)
  desc_end <- purrr::map_dbl(
    which(dive_start),
    ~ depth_peaks$i[depth_peaks$i > .x][1]
  )

  # Find steep regions in descent phases
  desc_phases <- data.frame(
    begin = seq_along(depth)[dive_start],
    end = desc_end
  ) %>%
    dplyr::mutate(desc = factor(dplyr::row_number())) %>%
    dplyr::group_by(desc) %>%
    dplyr::group_modify(
      function(row, key) {
        i <- row$begin:row$end
        data.frame(i, pitch = pitch[i])
      }
    ) %>%
    dplyr::filter(pitch <= pitch_thr)

  data.frame(depth,
             i = seq_along(depth)) %>%
    dplyr::left_join(desc_phases, by = "i") %>%
    dplyr::select(i, depth, pitch, desc)
}

#' Find constant pitch periods within descent
#'
#' Looks for periods in the descent with the lowest mean absolute error (MAE)
#' from a constant pitch. Finds up to `max_flat` periods, ranked by lowest MAE,
#' excluding any with MAE greater than the `mae_thr` threshold.
#'
#' @examples
#' const_pitch <- find_desc(prh$depth, prh$pitch, prh$fs) %>%
#'   tidyr::drop_na(desc) %>%
#'   dplyr::group_by(desc) %>%
#'   dplyr::group_modify(~ find_const_pitch(.x, .y,
#'                                         width = prh$fs,
#'                                         fs = prh$fs)) %>%
#'   dplyr::ungroup()
find_const_pitch <- function(data,
                             desc_id,
                             width,
                             fs,
                             max_flat = 20,
                             mae_thr = 0.5 * pi / 180) {
  pitch <- data$pitch

  # Calculate mean absolute error from a flat pitch
  get_flat_mae <- function(start) {
    this_pitch <- pitch[start:(start + width - 1)]
    mean(abs(this_pitch - mean(this_pitch)))
  }

  # Iteratively find lowest MAE periods
  flat_start <- rep(NA, max_flat)
  flat_mae <- double(max_flat)
  i <- seq_along(pitch)
  for (f in 1:max_flat) {
    # Stop if there are no available sequences longer than width
    avail_rle <- rle(is.finite(pitch))
    if (max(avail_rle$length[avail_rle$values]) < width) {
      break
    }
    flat_start[f] <- which.min(purrr::map_dbl(i, get_flat_mae))
    pitch[flat_start[f]:(flat_start[f] + width - 1)] <- Inf
  }
  flat_start <- flat_start[flat_mae <= mae_thr]
  flat_start <- na.omit(flat_start)

  # If no flat periods found (e.g. descent phase shorter than width), return
  # all NAs for flat phases
  data$flat <- rep(NA, nrow(data))
  if (length(flat_start) == 0) {
    return(data)
  }

  # Annotate flat phases in desc data frame
  for (s in 1:length(flat_start)) {
    fstart <- flat_start[s]
    fend <- fstart + width - 1
    data$flat[fstart:fend] <- s
  }
  data
}
