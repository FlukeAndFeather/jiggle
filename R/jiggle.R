speed_from_jiggle <- function(jiggle) {
  #ocdr = ae^(b(c1Jx+c2Jy+c3Jz)
  mult_exp <- function(Jx, Jy, Jz, a, b, c1, c2, c3) {
    a * exp(b * (c1 * Jx + c2 * Jy + c3 * Jz))
  }
  mult_exp_err <- function(par) {
    a <- par["a"]
    b <- par["b"]
    c1 <- par["c1"]
    c2 <- par["c2"]
    c3 <- par["c3"]
    with(jiggle, {
      rms(ocdr - mult_exp(Jx, Jy, Jz, a, b, c1, c2, c3))
    })
  }

  jiggle_mod <- optim(
    fn = mult_exp_err,
    par =   c(a = 2, b = 0.01, c1 = 0.5, c2 = 0.5, c3 = 0.5),
    lower = c( 1e-2,     1e-4,        0,        0,        0),
    upper = c(  1e2,        1,        1,        1,        1),
    method = "L-BFGS-B"
  )

  function(Jx, Jy, Jz) {
    par <- jiggle_mod$par
    a <- par["a"]
    b <- par["b"]
    c1 <- par["c1"]
    c2 <- par["c2"]
    c3 <- par["c3"]
    mult_exp(Jx, Jy, Jz, a, b, c1, c2, c3)
  }
}

get_jiggle <- function(ocdr, A, Afs, fs, low = 10, high = 90) {
  # Convert index between sampling rates
  fs_to_Afs <- function(i) {
    (i - 1) * Afs / fs + 1
  }

  # Filtered acceleration norm
  A_filt <- filter_acc(A, Afs, low, high)

  # RMS of acceleration in [10 90] Hz in each OCDR phase (dB scale)
  jiggle <- mapply(
    function(start, end) {
      apply(A_filt[start:end, ], 2, function(col) 20 * log10(rms(col)))
    },
    fs_to_Afs(ocdr$fstart),
    fs_to_Afs(ocdr$fend)
  ) %>%
    t()

  result <- ocdr
  result$Jx <- jiggle[, 1]
  result$Jy <- jiggle[, 2]
  result$Jz <- jiggle[, 3]

  result
}

#' Band pass filter acceleration norm
filter_acc <- function(A, Afs, low, high) {
  nyquist <- Afs / 2
  freq <- c(low, high) / nyquist
  h <- signal::fir1(127, freq, "pass")
  apply(A, 2, function(col) signal::filtfilt(h, col))
}

rms <- function(x) {
  sqrt(sum((x - mean(x))^2))
}
