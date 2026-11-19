#' Descriptive statistics
#'
#' Computes the number of observations, mean, median, standard deviation,
#' standard error of the mean, coefficient of variation and the 95\%
#' confidence interval of the mean, for the whole variable or for each group.
#'
#' @param y Numeric vector.
#' @param x Optional vector of groups (same length as \code{y}). When omitted,
#'   statistics are computed for the whole variable.
#' @param boxplot Logical. If \code{TRUE}, a boxplot is drawn.
#' @param xlab,ylab Axis labels of the boxplot.
#' @param graph.col Color of the boxes.
#' @return Invisibly, a list with the element \code{Result}, a data frame
#'   with the statistics (printed to the console).
#' @author Cicero Carlos de Souza Almeida
#' @examples
#' y <- c(3, 4, 4, 4, 5, 5, 6, 8, 8, 9, 9, 10, 11, 12)
#' desc(y)
#' g <- rep(c("A", "B"), each = 7)
#' desc(y, g, boxplot = TRUE)
#' @export
desc <- function(y, x = 1, boxplot = FALSE, xlab = "Treatments",
                 ylab = "Variable", graph.col = "skyblue") {
  name.y <- paste(deparse(substitute(y)), collapse = "")
  if (!is.numeric(y)) stop("'y' must be a numeric variable", call. = FALSE)

  stats1 <- function(v) {
    v <- v[!is.na(v)]
    n <- length(v)
    m <- mean(v)
    s <- if (n > 1) stats::sd(v) else NA_real_
    se <- s / sqrt(n)
    e <- if (n > 1) stats::qt(0.975, df = n - 1) * se else NA_real_
    c(N = n, Mean = m, Median = stats::median(v), sd = s, "Mean error" = se,
      "Coefficient of Variation (%)" = 100 * s / m,
      "CI 95% lower" = m - e, "CI 95% upper" = m + e)
  }

  cat("\n", .line(), "\n   Descriptive statistics for ", name.y,
      " variable\n", .line(), "\n", sep = "")

  if (length(x) == 1) {
    output <- data.frame(Statistics = round(stats1(y), 3))
    if (isTRUE(boxplot)) {
      graphics::boxplot(y, xlab = xlab, ylab = ylab, col = graph.col)
    }
  } else {
    if (length(x) != length(y)) {
      stop("'x' and 'y' must have the same length", call. = FALSE)
    }
    g <- factor(x)
    output <- as.data.frame(round(sapply(split(y, g), stats1), 3))
    if (isTRUE(boxplot)) {
      graphics::boxplot(y ~ g, xlab = xlab, ylab = ylab, col = graph.col)
    }
  }
  print(output)
  cat("\n", .line(), "\n", sep = "")
  invisible(list(Result = output))
}

#' Frequency distribution table
#'
#' Builds a frequency distribution table (absolute, relative and cumulative
#' relative frequencies) for a numeric variable.
#'
#' @param y Numeric vector.
#' @param bins Either the number of classes or a vector of break points,
#'   as accepted by \code{\link[base]{cut}}.
#' @return A data frame with the columns \code{Class}, \code{Class limits},
#'   \code{Frequency}, \code{Rel.Freq} and \code{Cum.Rel.Freq}.
#' @author Cicero Carlos de Souza Almeida
#' @seealso \code{\link{histg}}
#' @examples
#' y <- c(3, 4, 4, 4, 5, 5, 6, 8, 8, 9, 9, 10, 11, 12)
#' freqtable(y, 4)
#' freqtable(y, c(2, 5, 8, 11, 14))
#' @export
freqtable <- function(y, bins) {
  if (!is.numeric(y)) stop("'y' must be numeric", call. = FALSE)
  y <- y[!is.na(y)]
  Scores <- cut(y, bins, include.lowest = TRUE)
  freq <- as.vector(table(Scores))
  rel <- freq / sum(freq)
  table.y <- data.frame(Class = seq_along(freq),
                        "Class limits" = levels(Scores),
                        Frequency = freq,
                        Rel.Freq = round(rel, 2),
                        Cum.Rel.Freq = round(cumsum(rel), 2),
                        check.names = FALSE)
  cat("\nFrequency Distribution Table .... ok\n")
  cat(.line(), "\n", sep = "")
  table.y
}

#' Histogram with frequency table
#'
#' Draws a histogram and returns the frequency distribution table, with the
#' number of classes chosen by one of several rules.
#'
#' @param y Numeric vector.
#' @param xlab,ylab Axis labels.
#' @param graph.col Color of the bars.
#' @param scores Rule used to choose the number of classes: \code{"empiric"}
#'   (square root of n), \code{"sturges"}, \code{"ricerule"}, \code{"scott"}
#'   or \code{"fd"} (Freedman-Diaconis).
#' @param border Color of the borders of the bars (\code{TRUE} uses the
#'   default color).
#' @param freq Logical. If \code{TRUE}, frequencies are drawn; if
#'   \code{FALSE}, densities.
#' @param graph.type \code{"base"} (default) or \code{"ggplot2"}.
#' @param main Title of the graph.
#' @return A list with the element \code{Dist.Freq}, the frequency table
#'   (see \code{\link{freqtable}}).
#' @author Cicero Carlos de Souza Almeida
#' @examples
#' y <- c(3, 4, 4, 4, 5, 5, 6, 8, 8, 9, 9, 10, 11, 12)
#' histg(y)
#' histg(y, scores = "sturges")
#' @export
histg <- function(y, xlab = "Distribution", ylab = "Frequency",
                  graph.col = "turquoise", scores = "empiric", border = TRUE,
                  freq = TRUE, graph.type = "base", main = "") {
  scores <- match.arg(scores, c("empiric", "sturges", "ricerule", "scott", "fd"))
  graph.type <- match.arg(graph.type, c("base", "ggplot2"))
  if (!is.numeric(y)) stop("'y' must be numeric", call. = FALSE)
  y <- y[!is.na(y)]
  n <- length(y)
  if (n < 2) stop("at least two non-missing values are required", call. = FALSE)
  A <- diff(range(y))

  bins <- switch(scores,
    empiric = ceiling(sqrt(n)),
    sturges = ceiling(1 + log2(n)),
    ricerule = ceiling(2 * n^(1 / 3)),
    scott = ceiling(A / (3.5 * stats::sd(y) / n^(1 / 3))),
    fd = ceiling(A / (2 * stats::IQR(y) / n^(1 / 3))))
  if (!is.finite(bins) || bins < 1) bins <- ceiling(sqrt(n))

  # Class width slightly larger than A/bins so that the classes, centred on
  # the minimum, cover the whole range of the data.
  a <- A / bins
  af <- a + a / bins
  if (af <= 0) af <- 1
  brk <- seq(min(y) - af / 2, max(y) + af / 2, by = af)
  while (max(brk) < max(y)) brk <- c(brk, max(brk) + af)

  cat("\n", .line(), "\nHistogram .... ok\n", .line(), "\n", sep = "")
  table.y <- freqtable(y, brk)

  if (graph.type == "base") {
    graphics::hist(y, breaks = brk, border = border, col = graph.col,
                   xlab = xlab, ylab = ylab, freq = freq, main = main)
  } else {
    df <- data.frame(y = y)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$y)) +
      ggplot2::geom_histogram(breaks = brk, fill = graph.col,
                              color = "#e9ecef", alpha = 0.9) +
      ggplot2::labs(x = xlab, y = ylab, title = main) +
      ggplot2::theme_bw()
    print(p)
  }
  list(Dist.Freq = table.y)
}
