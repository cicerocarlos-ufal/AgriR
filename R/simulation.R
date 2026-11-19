#' Didactic graphs: confidence intervals and comparison of two means
#'
#' Graphs used to teach confidence intervals and the comparison of two
#' means (also used by the interactive application, \code{\link{run_app}}).
#' \itemize{
#'   \item \code{aproximate}: simulates a normal population with the given
#'     mean and standard deviation and shades the confidence interval of the
#'     mean for a sample of size \code{n}.
#'   \item \code{int.confidance}: histogram of a sample with the confidence
#'     interval of its mean.
#'   \item \code{means.test}: compares two means given their standard
#'     deviations and sample sizes (simulated populations).
#'   \item \code{par.test}: compares the means of two samples.
#' }
#' In \code{means.test} and \code{par.test} the least significant difference
#' is \eqn{t_{\alpha/2, n_1 + n_2 - 2} s_p \sqrt{1/n_1 + 1/n_2}}, where
#' \eqn{s_p^2} is the pooled variance; the means are considered different
#' when the absolute difference is larger than this value.
#'
#' @param mean,sd,n Mean, standard deviation and sample size.
#' @param dist Distribution used for the interval: \code{"normal"} or
#'   \code{"t"}.
#' @param alpha Significance level (the confidence level is
#'   \code{1 - alpha}).
#' @param variable Numeric vector (a sample).
#' @param mean1,mean2,sd1,sd2,n1,n2 Means, standard deviations and sample
#'   sizes of the two groups.
#' @param data1,data2 Numeric vectors with the two samples.
#' @return \code{aproximate} and \code{int.confidance}: invisibly, a list
#'   with the mean and the limits of the interval (\code{int.confidance}
#'   also returns the Shapiro-Wilk test). \code{means.test} and
#'   \code{par.test}: invisibly, a list with the difference between the
#'   means, the least significant difference and whether the difference is
#'   significant (\code{par.test} also returns the t test).
#' @author Cicero Carlos de Souza Almeida
#' @examples
#' aproximate(100, 14, 10)
#' set.seed(2)
#' x <- rnorm(30, 50, 5)
#' int.confidance(x)
#' means.test(120, 100, 20, 20, 10, 10)
#' par.test(rnorm(15, 50, 5), rnorm(15, 56, 5))
#' @name simulation
NULL

.ci_labels <- function(inf, sup, m, ytop, dx) {
  graphics::text(inf - dx, ytop, paste("Lower limit -", round(inf, 2)),
                 col = "blue", adj = 1)
  graphics::text(sup + dx, ytop, paste("Upper limit -", round(sup, 2)),
                 col = "blue", adj = 0)
  graphics::text(m, ytop * 1.08, paste("Mean -", round(m, 2)), col = "red",
                 adj = 0.5)
  graphics::abline(v = c(inf, m, sup), col = "blue")
}

#' @rdname simulation
#' @export
aproximate <- function(mean, sd, n, dist = "normal", alpha = 0.05) {
  dist <- match.arg(dist, c("normal", "t"))
  x <- sort(stats::rnorm(1000, mean, sd))
  hx <- stats::dnorm(x, mean, sd)
  ytop <- max(hx)
  graphics::hist(x, freq = FALSE, col = grDevices::rgb(1, 0, 0, 0.5),
                 density = 4, xlim = c(mean - 4 * sd, mean + 4 * sd),
                 ylim = c(0, ytop * 1.2), main = NULL, xlab = "Variable X")
  q <- if (dist == "normal") stats::qnorm(1 - alpha / 2) else
    stats::qt(1 - alpha / 2, df = n - 1)
  error <- q * sd / sqrt(n)
  inf <- mean - error
  sup <- mean + error
  i <- x >= inf & x <= sup
  graphics::lines(x, hx)
  graphics::polygon(c(inf, x[i], sup), c(0, hx[i], 0),
                    col = grDevices::rgb(0, 0, 255, maxColorValue = 255,
                                         alpha = 125))
  .ci_labels(inf, sup, mean, ytop, 0.1 * sd)
  invisible(list(mean = mean, lower = inf, upper = sup))
}

#' @rdname simulation
#' @export
int.confidance <- function(variable, dist = "t", alpha = 0.05) {
  dist <- match.arg(dist, c("normal", "t"))
  variable <- variable[!is.na(variable)]
  mean1 <- mean(variable)
  sd1 <- stats::sd(variable)
  n1 <- length(variable)
  x <- sort(stats::rnorm(1000, mean1, sd1))
  hx <- stats::dnorm(x, mean1, sd1)
  histg(variable,
        graph.col = grDevices::rgb(0, 0, 255, maxColorValue = 255, alpha = 80),
        freq = FALSE)
  q <- if (dist == "normal") stats::qnorm(1 - alpha / 2) else
    stats::qt(1 - alpha / 2, df = n1 - 1)
  error <- q * sd1 / sqrt(n1)
  inf <- mean1 - error
  sup <- mean1 + error
  i <- x >= inf & x <= sup
  graphics::lines(x, hx)
  graphics::polygon(c(inf, x[i], sup), c(0, hx[i], 0),
                    col = grDevices::rgb(0, 0, 255, maxColorValue = 255,
                                         alpha = 125))
  .ci_labels(inf, sup, mean1, max(hx), 0)
  sh <- stats::shapiro.test(variable)
  cat(.line(), "\nMean: ", round(mean1, 4), "\n", sep = "")
  cat("Confidence interval (", 100 * (1 - alpha), "%): ", round(inf, 4),
      " - ", round(sup, 4), "\n", .line(), "\n", sep = "")
  print(sh)
  invisible(list(mean = mean1, lower = inf, upper = sup, shapiro = sh))
}

# Draws the two distributions and the information of the test.
.two_means_plot <- function(x1, x2, mean1, mean2, sd1, sd2, error, bands) {
  x11 <- sort(stats::rnorm(1000, mean1, sd1))
  x22 <- sort(stats::rnorm(1000, mean2, sd2))
  hx11 <- stats::dnorm(x11, mean1, sd1)
  hx22 <- stats::dnorm(x22, mean2, sd2)
  xlim <- range(c(x1, x2, x11, x22))
  xlim <- xlim + c(-1, 1) * 0.1 * diff(xlim)
  d1 <- graphics::hist(x1, plot = FALSE)$density
  d2 <- graphics::hist(x2, plot = FALSE)$density
  ytop <- max(c(d1, d2, hx11, hx22)) * 1.4
  graphics::hist(x1, freq = FALSE, col = grDevices::rgb(1, 0, 0, 0.5),
                 density = 4, xlab = "", main = NULL, ylim = c(0, ytop),
                 xlim = xlim)
  graphics::hist(x2, freq = FALSE, col = grDevices::rgb(0, 0, 1, 0.5),
                 density = 4, add = TRUE)
  for (g in 1:2) {
    xx <- if (g == 1) x11 else x22
    hh <- if (g == 1) hx11 else hx22
    m <- if (g == 1) mean1 else mean2
    cl <- if (g == 1) "red" else "blue"
    inf <- m - error
    sup <- m + error
    i <- xx >= inf & xx <= sup
    graphics::lines(xx, hh, col = cl)
    graphics::polygon(c(inf, xx[i], sup), c(0, hh[i], 0),
                      col = grDevices::adjustcolor(cl, alpha.f = 0.5))
    graphics::abline(v = if (bands) c(inf, m, sup) else m, col = cl)
  }
  dif <- mean1 - mean2
  signif <- abs(dif) > error
  x0 <- xlim[1]
  graphics::text(x0, ytop, "Test information", col = "blue", adj = 0)
  graphics::text(x0, ytop * 0.92, paste(round(error, 2), "- LSD"), col = "blue",
                 adj = 0)
  graphics::text(x0, ytop * 0.85, paste(round(dif, 2), "- Mean difference"),
                 col = "blue", adj = 0)
  graphics::text(x0, ytop * 0.77,
                 if (signif) "Statistically significant difference" else
                   "No statistically significant difference",
                 col = if (signif) "blue" else "red", adj = 0)
  signif
}

.lsd_two <- function(sd1, sd2, n1, n2, alpha) {
  var_pod <- ((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2)
  sdp <- sqrt(var_pod * (1 / n1 + 1 / n2))
  stats::qt(1 - alpha / 2, df = n1 + n2 - 2) * sdp
}

#' @rdname simulation
#' @export
means.test <- function(mean1, mean2, sd1, sd2, n1, n2, alpha = 0.05) {
  error <- .lsd_two(sd1, sd2, n1, n2, alpha)
  x1 <- stats::rnorm(1000, mean1, sd1)
  x2 <- stats::rnorm(1000, mean2, sd2)
  signif <- .two_means_plot(x1, x2, mean1, mean2, sd1, sd2, error, TRUE)
  invisible(list(difference = mean1 - mean2, LSD = error,
                 significant = signif))
}

#' @rdname simulation
#' @export
par.test <- function(data1, data2, alpha = 0.05) {
  data1 <- data1[!is.na(data1)]
  data2 <- data2[!is.na(data2)]
  mean1 <- mean(data1)
  mean2 <- mean(data2)
  sd1 <- stats::sd(data1)
  sd2 <- stats::sd(data2)
  error <- .lsd_two(sd1, sd2, length(data1), length(data2), alpha)
  signif <- .two_means_plot(data1, data2, mean1, mean2, sd1, sd2, error, FALSE)
  tt <- stats::t.test(data1, data2, var.equal = TRUE, conf.level = 1 - alpha)
  invisible(list(difference = mean1 - mean2, LSD = error,
                 significant = signif, t.test = tt))
}
