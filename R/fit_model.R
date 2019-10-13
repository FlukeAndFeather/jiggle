#' Fit jiggle model
#'
#' Multiple exponential regression of OCDR on the "jiggle", i.e. root mean
#' square amplitude of filtered triaxial acceleration.
#'
#' @param prh PRH data frame with \code{ocdr} and \code{rms_acc} columns
#' @param n_iter Number of iterations to try fitting the non-linear model
#'
#' @return PRH data frame with \code{speed} column. Adds an attribute,
#'   \code{speed_model}, a list with elements \code{a}, \code{b}, \code{c1},
#'   \code{c2}, \code{c3}, and \code{predict_speed}. The first five elements
#'   are the coefficients of the model and \code{predict_speed} is a function
#'   that takes an acceleration RMS matrix and returns the predicted speed.
#'
#' @examples
#' prh_ocdr <- get_ocdr(jgl_desc(prh_expl))
#' acc_filt <- jgl_filtacc(Araw_expl, 10, 90)
#' prh_rms <- get_rms_acc(prh_ocdr, acc_filt)
#' prh_spd <- jgl_fit(prh_rms)
#'
#' @export
jgl_fit <- function(prh, n_iter = 100) {
  if (!"rms_acc" %in% colnames(prh)) {
    stop("Acceleration RMS not found. Did you run `get_rms_acc`?")
  }

  prh_rms <- tidyr::drop_na(prh, ocdr)
  Jx <- prh_rms$rms_acc[, 1]
  Jy <- prh_rms$rms_acc[, 2]
  Jz <- prh_rms$rms_acc[, 3]

  #ocdr = ae^(b(c1Jx+c2Jy+c3Jz)
  mult_exp <- function(Jx, Jy, Jz, a, b, c1, c2, c3) {
    a * exp(b * (c1 * Jx + c2 * Jy + c3 * Jz))
  }
  # RMS error of regression
  mult_exp_err <- function(par) {
    a <- par[1]
    b <- par[2]
    c1 <- par[3]
    c2 <- par[4]
    c3 <- par[5]
    resid <- prh_rms$ocdr - mult_exp(Jx, Jy, Jz, a, b, c1, c2, c3)
    sqrt(sum(resid^2))
  }
  # Try to fit a model using numerical optimization
  try_fit <- function(a, b, c1, c2, c3) {
    tryCatch({
      stats::optim(
        fn = mult_exp_err,
        par =   c(a,    b,    c1, c2, c3),
        lower = c(1e-2, 1e-4, 0,  0,  0),
        upper = c(1e2,  1,    1,  1,  1),
        method = "L-BFGS-B"
      )
    }, error = function(e) list(convergence = NA, value = NA))
  }

  # Try fitting the model n_iter times with random starting parameters
  n_iter <- 1e3
  fit_results <- data.frame(
    a = stats::runif(n_iter, 0.1, 10),
    b = stats::rlnorm(n_iter, meanlog = -3, sdlog = 1),
    c1 = stats::runif(n_iter, 0, 1),
    c2 = stats::runif(n_iter, 0, 1),
    c3 = stats::runif(n_iter, 0, 1)
  )
  fit_results$output <- mapply(try_fit,
                               fit_results$a,
                               fit_results$b,
                               fit_results$c1,
                               fit_results$c2,
                               fit_results$c3,
                               SIMPLIFY = FALSE)
  fit_results$convergence <- sapply(fit_results$output,
                                    function(o) o$convergence)
  fit_results$value <- sapply(fit_results$output,
                              function(o) o$value)
  fit_results <- fit_results[fit_results$convergence == 0, ]
  fit_results <- fit_results[order(fit_results$value), ]

  # Choose the best fitting model
  a <- fit_results$output[[1]]$par[1]
  b <- fit_results$output[[1]]$par[2]
  c1 <- fit_results$output[[1]]$par[3]
  c2 <- fit_results$output[[1]]$par[4]
  c3 <- fit_results$output[[1]]$par[5]
  mod <- list(
    a,
    b,
    c1,
    c2,
    c3,
    predict_speed = function(rms_acc) {
      mult_exp(rms_acc[, 1], rms_acc[, 2], rms_acc[, 3],
               a, b, c1, c2, c3)
    }
  )

  # Return result
  result <- prh
  result$speed <- mod$predict_speed(prh$rms_acc)
  attr(result, "speed_model") <- mod
  result
}
