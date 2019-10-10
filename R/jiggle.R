# jgl_fit <- function(prh, acc, low, high, pitch_thr, depth_thr) {
#   ocdr <- prh %>%
#     find_desc(pitch_thr) %>%
#     --rank_pitch() %>%
#     calc_ocdr()
#
#   jgl <- acc %>%
#     filter_acc(low, high) %>%
#     calc_rms(ocdr)
#
#   jgl_model <- ocdr %>%
#     left_join(jgl) %>%
#     fit_model()
# }
