#' Graphs of multiple comparison tests
#'
#' Draw the means of the treatments with error bars (mean plus/minus one
#' standard deviation) and the letters of a multiple comparison test.
#' \code{barplotb} and \code{boxplotb} use base graphics; \code{barplotg} and
#' \code{pointplotg} use \pkg{ggplot2}.
#'
#' @param table.graph Data frame returned by the multiple comparison tests
#'   (see \code{\link{tukey}}): columns with the means, standard deviations,
#'   upper and lower limits of the error bars and the letters (in this
#'   order), with the treatments as row names.
#' @param y,trt Response variable and treatments (\code{boxplotb} only).
#' @param xlab,ylab Axis labels.
#' @param graph.col Color of the bars, boxes or points.
#' @param graph.den Density of the shading lines of the bars (\code{NULL}
#'   for solid bars).
#' @param main,title Title of the graph.
#' @param caption Caption of the graph (ggplot2 graphs).
#' @param barcolor Color of the error bars (ggplot2 graphs).
#' @return \code{barplotb}: invisibly, the midpoints of the bars.
#'   \code{boxplotb}: invisibly, the statistics used to draw the boxes.
#'   \code{barplotg} and \code{pointplotg}: invisibly, the ggplot object
#'   (the graph is printed).
#' @author Cicero Carlos de Souza Almeida
#' @seealso \code{\link{tukey}}
#' @examples
#' y <- c(25, 26, 24, 27, 30, 31, 29, 32, 35, 36, 34, 37)
#' trt <- rep(c("A", "B", "C"), each = 4)
#' tab <- tukey(y, trt, DFerror = 9, SSerror = 15, graph = FALSE)
#' barplotb(tab)
#' boxplotb(y, trt, tab)
#' barplotg(tab)
#' pointplotg(tab)
#' @name graphs
NULL

.tg_limits <- function(table.graph) {
  m <- table.graph[, 1]
  up <- table.graph[, 3]
  lo <- table.graph[, 4]
  up[is.na(up)] <- m[is.na(up)]
  lo[is.na(lo)] <- m[is.na(lo)]
  list(m = m, up = up, lo = lo,
       lab = gsub(" ", "", as.character(table.graph[, 5])))
}

#' @rdname graphs
#' @export
barplotb <- function(table.graph, xlab = "", ylab = "", graph.col = "skyblue",
                     graph.den = NULL, main = "") {
  L <- .tg_limits(table.graph)
  off <- .label_offset(table.graph[, 2], L$up)
  top <- max(0, L$up + 2 * off, L$up * 1.3)
  bottom <- min(0, L$lo)
  means <- L$m
  names(means) <- rownames(table.graph)
  bp <- graphics::barplot(means, xpd = FALSE, ylim = c(bottom, top),
                          xlab = xlab, ylab = ylab, lwd = 2, main = main,
                          las = 1, density = graph.den, col = graph.col)
  draw <- L$up > L$lo
  if (any(draw)) {
    graphics::arrows(bp[draw], L$up[draw], bp[draw], L$lo[draw], code = 3,
                     angle = 90, length = 0.05, col = "red", lwd = 2)
  }
  graphics::text(bp, L$up + off, L$lab, cex = 1.5)
  invisible(bp)
}

#' @rdname graphs
#' @export
boxplotb <- function(y, trt, table.graph, xlab = "", ylab = "",
                     graph.col = "skyblue", main = "") {
  trt <- factor(trt)
  b <- graphics::boxplot(y ~ trt, plot = FALSE)
  tg <- table.graph[match(b$names, rownames(table.graph)), , drop = FALSE]
  L <- .tg_limits(tg)
  b$stats[2, ] <- L$lo
  b$stats[3, ] <- L$m
  b$stats[4, ] <- L$up
  b$stats[1, ] <- pmin(b$stats[1, ], L$lo)
  b$stats[5, ] <- pmax(b$stats[5, ], L$up)
  off <- .label_offset(tg[, 2], b$stats[5, ])
  ypos <- b$stats[5, ] + off
  ylim <- range(c(b$stats, b$out, ypos + off), na.rm = TRUE)
  graphics::bxp(b, ylab = ylab, xlab = xlab, boxfill = graph.col, main = main,
                ylim = ylim)
  graphics::text(seq_along(b$names), ypos, L$lab, cex = 1.5)
  invisible(b)
}

.tg_df <- function(table.graph) {
  L <- .tg_limits(table.graph)
  off <- .label_offset(table.graph[, 2], L$up)
  data.frame(trt = factor(rownames(table.graph),
                          levels = rownames(table.graph)),
             mean = L$m, up = L$up, lo = L$lo, lab = L$lab,
             ylab = L$up + off)
}

#' @rdname graphs
#' @export
barplotg <- function(table.graph, xlab = "", ylab = "", title = "",
                     caption = "", graph.col = "skyblue", barcolor = "orange",
                     main = "") {
  if (!nzchar(title)) title <- main
  df <- .tg_df(table.graph)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$trt, y = .data$mean)) +
    ggplot2::geom_col(fill = graph.col, alpha = 0.7) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lo, ymax = .data$up),
                           width = 0.2, colour = barcolor, alpha = 0.9,
                           linewidth = 1.3) +
    ggplot2::geom_text(ggplot2::aes(y = .data$ylab, label = .data$lab),
                       size = 5) +
    ggplot2::labs(title = title, x = xlab, y = ylab, caption = caption) +
    ggplot2::theme_bw(base_size = 12)
  print(p)
  invisible(p)
}

#' @rdname graphs
#' @export
pointplotg <- function(table.graph, xlab = "", ylab = "", title = "",
                       caption = "", graph.col = "black", barcolor = "orange",
                       main = "") {
  if (!nzchar(title)) title <- main
  df <- .tg_df(table.graph)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$trt, y = .data$mean)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lo, ymax = .data$up),
                           width = 0.2, colour = barcolor, alpha = 0.9,
                           linewidth = 1.3) +
    ggplot2::geom_point(size = 5, colour = graph.col) +
    ggplot2::geom_text(ggplot2::aes(y = .data$ylab, label = .data$lab),
                       size = 5) +
    ggplot2::labs(title = title, x = xlab, y = ylab, caption = caption) +
    ggplot2::theme_bw(base_size = 12)
  print(p)
  invisible(p)
}
