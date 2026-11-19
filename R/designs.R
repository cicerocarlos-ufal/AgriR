#' Randomization and sketch of experimental designs
#'
#' Randomize the treatments of an experiment, draw a sketch of the field
#' layout and return the field book. Available designs: completely
#' randomized (\code{des.crd}), randomized blocks (\code{des.rbd}), double
#' factorial in CRD and RBD (\code{des.fat2.crd}, \code{des.fat2.rbd}) and
#' split-plot in CRD and RBD (\code{des.sp.crd}, \code{des.sp.rbd}).
#'
#' Treatments are labelled \code{T1}, \code{T2}, ... (one factor) or
#' \code{F1-i} and \code{F2-j} (two factors). In the sketches, each cell is
#' an experimental unit; in the block designs each column is a block, and
#' in the split-plot designs each column is a plot divided into split-plots.
#' Use \code{\link[base]{set.seed}} before calling the function to obtain a
#' reproducible randomization.
#'
#' @param trt Number of treatments.
#' @param factor1,factor2 Number of levels of factors 1 and 2.
#' @param rep Number of replications (or blocks).
#' @param col.1,col.2 Colors used to build the palette of the treatments.
#' @param graph.col Logical. If \code{TRUE} (default), the cells are
#'   colored by treatment; if \code{FALSE}, they are white.
#' @param fac.names Names of factors 1 and 2 (shown in the title).
#' @param save Logical. If \code{TRUE}, the sketch is also saved as a TIFF
#'   file in the working directory.
#' @param file Name of the TIFF file (used when \code{save = TRUE}).
#' @param graph Logical. If \code{FALSE}, nothing is drawn.
#' @return Invisibly, a list with \code{Design} (matrix with the layout as
#'   drawn), \code{Desing} (the same matrix, kept for compatibility with
#'   previous versions) and \code{Book} (field book: one row per
#'   experimental unit).
#' @author Cicero Carlos de Souza Almeida
#' @examples
#' set.seed(123)
#' des.crd(trt = 4, rep = 3)
#' des.rbd(trt = 4, rep = 3)
#' des.fat2.crd(2, 3, rep = 2, fac.names = c("Cultivar", "Dose"))
#' des.fat2.rbd(2, 3, rep = 2)
#' des.sp.crd(3, 2, rep = 2)
#' des.sp.rbd(3, 2, rep = 3)$Book
#' @name designs
NULL

# Draws a grid of cells. 'lbl' and 'cols' are matrices with the same shape.
.draw_grid <- function(lbl, cols, main = "", col.labels = NULL,
                       sep.cols = NULL, sep.rows = NULL) {
  nr <- nrow(lbl)
  nc <- ncol(lbl)
  op <- graphics::par(mar = c(0.5, 0.5, if (is.null(col.labels)) 2.5 else 4, 0.5))
  on.exit(graphics::par(op))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, nc), ylim = c(0, nr), xaxs = "i",
                        yaxs = "i")
  jj <- rep(seq_len(nc), each = nr)
  ii <- rep(seq_len(nr), times = nc)
  x0 <- jj - 1
  y0 <- nr - ii
  graphics::rect(x0, y0, x0 + 1, y0 + 1, col = as.vector(cols),
                 border = "gray30")
  labs <- as.vector(lbl)
  w <- max(graphics::strwidth(labs, cex = 1), 1e-6)
  h <- max(graphics::strheight(labs, cex = 1), 1e-6)
  cex <- max(0.3, min(1.4, 0.9 / w, 0.8 / h))
  graphics::text(x0 + 0.5, y0 + 0.5, as.vector(lbl), cex = cex)
  if (!is.null(sep.cols)) {
    graphics::abline(v = sep.cols, lwd = 3)
  }
  if (!is.null(sep.rows)) {
    graphics::abline(h = nr - sep.rows, lwd = 2)
  }
  graphics::rect(0, 0, nc, nr, lwd = 2)
  if (!is.null(col.labels)) {
    graphics::mtext(col.labels, side = 3, at = seq_len(nc) - 0.5, line = 0.3,
                    cex = min(0.9, cex))
  }
  graphics::title(main, line = if (is.null(col.labels)) 1 else 2.5)
  invisible(NULL)
}

.save_tiff <- function(save, file, width, height) {
  if (isTRUE(save)) {
    grDevices::dev.copy(grDevices::tiff, filename = file, width = width,
                        height = height, res = 100)
    grDevices::dev.off()
    message("Design saved in ", file.path(getwd(), file))
  }
  invisible(NULL)
}

.check_design_args <- function(...) {
  v <- c(...)
  if (any(!is.finite(v)) || any(v < 2) || any(v != round(v))) {
    stop("the numbers of treatments/levels and replications must be ",
         "integers >= 2", call. = FALSE)
  }
  invisible(TRUE)
}

.palette <- function(n, col.1, col.2, graph.col) {
  if (isTRUE(graph.col)) grDevices::colorRampPalette(c(col.1, col.2))(n)
  else rep("white", n)
}

# Replication number of each occurrence of a treatment.
.rep_index <- function(x) {
  as.integer(stats::ave(seq_along(x), x, FUN = seq_along))
}

#' @rdname designs
#' @export
des.crd <- function(trt, rep, col.1 = "gray", col.2 = "white",
                    graph.col = TRUE, save = FALSE, file = "des.crd.tiff",
                    graph = TRUE) {
  .check_design_args(trt, rep)
  t <- trt
  r <- rep
  labs <- paste0("T", seq_len(t))
  cols <- .palette(t, col.1, col.2, graph.col)
  idx <- sample(base::rep(seq_len(t), r))
  mat <- matrix(labs[idx], nrow = t, ncol = r, byrow = TRUE)
  colmat <- matrix(cols[idx], nrow = t, ncol = r, byrow = TRUE)
  if (isTRUE(graph)) {
    .draw_grid(mat, colmat, main = "Completely randomized design")
    .save_tiff(save, file, 200 * r, 200 * t)
  }
  treat <- labs[idx]
  book <- data.frame(plot = seq_along(idx), row = base::rep(seq_len(t), each = r),
                     column = base::rep(seq_len(r), times = t), treat = treat,
                     rep = .rep_index(treat))
  print(mat)
  invisible(list(Design = mat, Desing = mat, Book = book))
}

#' @rdname designs
#' @export
des.rbd <- function(trt, rep, col.1 = "gray", col.2 = "white",
                    graph.col = TRUE, save = FALSE, file = "des.rbd.tiff",
                    graph = TRUE) {
  .check_design_args(trt, rep)
  t <- trt
  r <- rep
  labs <- paste0("T", seq_len(t))
  cols <- .palette(t, col.1, col.2, graph.col)
  idx <- matrix(0L, nrow = t, ncol = r)
  for (b in seq_len(r)) idx[, b] <- sample(seq_len(t))
  mat <- matrix(labs[idx], nrow = t, ncol = r)
  colnames(mat) <- paste0("Block-", seq_len(r))
  colmat <- matrix(cols[idx], nrow = t, ncol = r)
  if (isTRUE(graph)) {
    .draw_grid(mat, colmat, main = "Randomized block design",
               col.labels = colnames(mat))
    .save_tiff(save, file, 200 * r, 200 * t)
  }
  book <- data.frame(plot = seq_len(t * r),
                     Block = base::rep(seq_len(r), each = t),
                     position = base::rep(seq_len(t), times = r),
                     treat = as.vector(mat))
  print(mat)
  invisible(list(Design = mat, Desing = mat, Book = book))
}

# Treatments of a double factorial (factor 1 varies slowest).
.fat2_treatments <- function(f1, f2) {
  g <- expand.grid(F2 = seq_len(f2), F1 = seq_len(f1))
  data.frame(F1 = paste0("F1-", g$F1), F2 = paste0("F2-", g$F2),
             label = paste0("F1-", g$F1, " | F2-", g$F2),
             stringsAsFactors = FALSE)
}

.fac_title <- function(title, fac.names) {
  paste0(title, "\nF1 = ", fac.names[1], "   |   F2 = ", fac.names[2])
}

#' @rdname designs
#' @export
des.fat2.crd <- function(factor1, factor2, rep, col.1 = "gray",
                         col.2 = "white", graph.col = TRUE,
                         fac.names = c("F1", "F2"), save = FALSE,
                         file = "des.fat2.crd.tiff", graph = TRUE) {
  .check_design_args(factor1, factor2, rep)
  tr <- .fat2_treatments(factor1, factor2)
  nt <- nrow(tr)
  r <- rep
  cols <- .palette(nt, col.1, col.2, graph.col)
  idx <- sample(base::rep(seq_len(nt), r))
  mat <- matrix(tr$label[idx], nrow = nt, ncol = r, byrow = TRUE)
  colmat <- matrix(cols[idx], nrow = nt, ncol = r, byrow = TRUE)
  if (isTRUE(graph)) {
    .draw_grid(mat, colmat,
               main = .fac_title("Factorial in completely randomized design",
                                 fac.names))
    .save_tiff(save, file, 200 * r, 60 * nt)
  }
  book <- data.frame(plot = seq_along(idx),
                     row = base::rep(seq_len(nt), each = r),
                     column = base::rep(seq_len(r), times = nt),
                     factor1 = tr$F1[idx], factor2 = tr$F2[idx],
                     rep = .rep_index(idx))
  print(mat)
  invisible(list(Design = mat, Desing = mat, Book = book))
}

#' @rdname designs
#' @export
des.fat2.rbd <- function(factor1, factor2, rep, col.1 = "gray",
                         col.2 = "white", graph.col = TRUE,
                         fac.names = c("F1", "F2"), save = FALSE,
                         file = "des.fat2.rbd.tiff", graph = TRUE) {
  .check_design_args(factor1, factor2, rep)
  tr <- .fat2_treatments(factor1, factor2)
  nt <- nrow(tr)
  r <- rep
  cols <- .palette(nt, col.1, col.2, graph.col)
  idx <- matrix(0L, nrow = nt, ncol = r)
  for (b in seq_len(r)) idx[, b] <- sample(seq_len(nt))
  mat <- matrix(tr$label[idx], nrow = nt, ncol = r)
  colnames(mat) <- paste0("Block-", seq_len(r))
  colmat <- matrix(cols[idx], nrow = nt, ncol = r)
  if (isTRUE(graph)) {
    .draw_grid(mat, colmat,
               main = .fac_title("Factorial in randomized block design",
                                 fac.names),
               col.labels = colnames(mat))
    .save_tiff(save, file, 200 * r, 60 * nt)
  }
  book <- data.frame(plot = seq_len(nt * r),
                     Block = base::rep(seq_len(r), each = nt),
                     position = base::rep(seq_len(nt), times = r),
                     factor1 = tr$F1[as.vector(idx)],
                     factor2 = tr$F2[as.vector(idx)])
  print(mat)
  invisible(list(Design = mat, Desing = mat, Book = book))
}

# Split-plot layout: 'plots' is the vector of factor 1 levels (one per plot,
# in field order); each plot receives a random order of the f2 split-plots.
.split_layout <- function(plots, f2, col.1, col.2, graph.col) {
  np <- length(plots)
  cols <- .palette(f2, col.1, col.2, graph.col)
  sub <- matrix(0L, nrow = f2, ncol = np)
  for (p in seq_len(np)) sub[, p] <- sample(seq_len(f2))
  f1lab <- paste0("F1-", plots)
  lbl <- matrix(paste0(base::rep(f1lab, each = f2), "\nF2-", as.vector(sub)),
                nrow = f2, ncol = np)
  colmat <- matrix(cols[as.vector(sub)], nrow = f2, ncol = np)
  list(sub = sub, f1lab = f1lab, lbl = lbl, colmat = colmat)
}

#' @rdname designs
#' @export
des.sp.crd <- function(factor1, factor2, rep, col.1 = "gray", col.2 = "white",
                       graph.col = TRUE, fac.names = c("F1", "F2"),
                       save = FALSE, file = "des.sp.crd.tiff", graph = TRUE) {
  .check_design_args(factor1, factor2, rep)
  f1 <- factor1
  f2 <- factor2
  r <- rep
  plots <- sample(base::rep(seq_len(f1), r))
  L <- .split_layout(plots, f2, col.1, col.2, graph.col)
  mat <- matrix(gsub("\n", " | ", L$lbl), nrow = f2)
  colnames(mat) <- paste0("Plot-", seq_along(plots))
  if (isTRUE(graph)) {
    .draw_grid(L$lbl, L$colmat,
               main = .fac_title("Split-plot in completely randomized design",
                                 fac.names),
               col.labels = paste0("P", seq_along(plots)))
    .save_tiff(save, file, 120 * length(plots), 120 * f2)
  }
  book <- data.frame(plot = base::rep(seq_along(plots), each = f2),
                     subplot = base::rep(seq_len(f2), times = length(plots)),
                     factor1 = base::rep(L$f1lab, each = f2),
                     factor2 = paste0("F2-", as.vector(L$sub)),
                     rep = base::rep(.rep_index(plots), each = f2))
  print(mat)
  invisible(list(Design = mat, Desing = mat, Book = book))
}

#' @rdname designs
#' @export
des.sp.rbd <- function(factor1, factor2, rep, col.1 = "gray", col.2 = "white",
                       graph.col = TRUE, fac.names = c("F1", "F2"),
                       save = FALSE, file = "des.sp.rbd.tiff", graph = TRUE) {
  .check_design_args(factor1, factor2, rep)
  f1 <- factor1
  f2 <- factor2
  r <- rep
  plots <- as.vector(vapply(seq_len(r), function(b) sample(seq_len(f1)),
                            integer(f1)))
  block <- base::rep(seq_len(r), each = f1)
  L <- .split_layout(plots, f2, col.1, col.2, graph.col)
  mat <- matrix(gsub("\n", " | ", L$lbl), nrow = f2)
  colnames(mat) <- paste0("Block-", block, ".Plot-",
                          base::rep(seq_len(f1), times = r))
  if (isTRUE(graph)) {
    .draw_grid(L$lbl, L$colmat,
               main = .fac_title("Split-plot in randomized block design",
                                 fac.names),
               col.labels = paste0("B", block),
               sep.cols = if (r > 1) seq_len(r - 1) * f1 else NULL)
    .save_tiff(save, file, 120 * length(plots), 120 * f2)
  }
  book <- data.frame(Block = base::rep(block, each = f2),
                     plot = base::rep(seq_along(plots), each = f2),
                     subplot = base::rep(seq_len(f2), times = length(plots)),
                     factor1 = base::rep(L$f1lab, each = f2),
                     factor2 = paste0("F2-", as.vector(L$sub)))
  print(mat)
  invisible(list(Design = mat, Desing = mat, Book = book))
}
