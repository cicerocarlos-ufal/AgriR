#' Randomization and sketch of designs with three factors
#'
#' Randomize the treatments of experiments with three factors, draw a sketch
#' of the field layout and return the field book: triple factorial in the
#' completely randomized design (\code{des.fat3.crd}) or in randomized
#' blocks (\code{des.fat3.rbd}), and split-split-plot in the completely
#' randomized design (\code{des.ssp.crd}) or in randomized blocks
#' (\code{des.ssp.rbd}).
#'
#' In the split-split-plot designs, the levels of factor 1 are randomized to
#' the plots, the levels of factor 2 to the split-plots inside each plot and
#' the levels of factor 3 to the split-split-plots inside each split-plot.
#' In the sketch each column is a plot; thin horizontal lines separate the
#' split-plots and the cells are colored by the level of factor 2. In the
#' block designs, thick vertical lines separate the blocks.
#'
#' @param factor1,factor2,factor3 Number of levels of factors 1, 2 and 3.
#' @param rep Number of replications (or blocks).
#' @param fac.names Names of the three factors (shown in the title).
#' @inheritParams designs
#' @return Invisibly, a list with \code{Design} (matrix with the layout as
#'   drawn) and \code{Book} (field book: one row per experimental unit).
#' @author Cicero Carlos de Souza Almeida
#' @seealso \code{\link{des.fat2.crd}}, \code{\link{des.sp.crd}},
#'   \code{\link{anova_fat3_crd}}, \code{\link{anova_ssp_crd}}
#' @examples
#' set.seed(1)
#' des.fat3.crd(2, 2, 2, rep = 3)
#' des.fat3.rbd(2, 3, 2, rep = 2, fac.names = c("Cultivar", "Dose", "Spacing"))
#' des.ssp.crd(2, 2, 3, rep = 2)
#' d <- des.ssp.rbd(3, 2, 2, rep = 2)
#' head(d$Book)
#' @name designs3
NULL

.fat3_treatments <- function(f1, f2, f3) {
  g <- expand.grid(F3 = seq_len(f3), F2 = seq_len(f2), F1 = seq_len(f1))
  data.frame(F1 = paste0("F1-", g$F1), F2 = paste0("F2-", g$F2),
             F3 = paste0("F3-", g$F3),
             label = paste0("F1-", g$F1, " | F2-", g$F2, " | F3-", g$F3),
             stringsAsFactors = FALSE)
}

.fac3_title <- function(title, fac.names) {
  fac.names <- rep_len(as.character(fac.names), 3)
  paste0(title, "\nF1 = ", fac.names[1], "   |   F2 = ", fac.names[2],
         "   |   F3 = ", fac.names[3])
}

#' @rdname designs3
#' @export
des.fat3.crd <- function(factor1, factor2, factor3, rep, col.1 = "gray",
                         col.2 = "white", graph.col = TRUE,
                         fac.names = c("F1", "F2", "F3"), save = FALSE,
                         file = "des.fat3.crd.tiff", graph = TRUE) {
  .check_design_args(factor1, factor2, factor3, rep)
  tr <- .fat3_treatments(factor1, factor2, factor3)
  nt <- nrow(tr)
  r <- rep
  cols <- .palette(nt, col.1, col.2, graph.col)
  idx <- sample(base::rep(seq_len(nt), r))
  mat <- matrix(tr$label[idx], nrow = nt, ncol = r, byrow = TRUE)
  colmat <- matrix(cols[idx], nrow = nt, ncol = r, byrow = TRUE)
  if (isTRUE(graph)) {
    .draw_grid(mat, colmat,
               main = .fac3_title("Triple factorial in completely randomized design",
                                  fac.names))
    .save_tiff(save, file, 250 * r, 50 * nt)
  }
  book <- data.frame(plot = seq_along(idx),
                     row = base::rep(seq_len(nt), each = r),
                     column = base::rep(seq_len(r), times = nt),
                     factor1 = tr$F1[idx], factor2 = tr$F2[idx],
                     factor3 = tr$F3[idx], rep = .rep_index(idx))
  print(mat)
  invisible(list(Design = mat, Book = book))
}

#' @rdname designs3
#' @export
des.fat3.rbd <- function(factor1, factor2, factor3, rep, col.1 = "gray",
                         col.2 = "white", graph.col = TRUE,
                         fac.names = c("F1", "F2", "F3"), save = FALSE,
                         file = "des.fat3.rbd.tiff", graph = TRUE) {
  .check_design_args(factor1, factor2, factor3, rep)
  tr <- .fat3_treatments(factor1, factor2, factor3)
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
               main = .fac3_title("Triple factorial in randomized block design",
                                  fac.names),
               col.labels = colnames(mat))
    .save_tiff(save, file, 250 * r, 50 * nt)
  }
  v <- as.vector(idx)
  book <- data.frame(plot = seq_len(nt * r),
                     Block = base::rep(seq_len(r), each = nt),
                     position = base::rep(seq_len(nt), times = r),
                     factor1 = tr$F1[v], factor2 = tr$F2[v], factor3 = tr$F3[v])
  print(mat)
  invisible(list(Design = mat, Book = book))
}

# Split-split-plot layout. 'plots' has the level of factor 1 of each plot
# (in field order). Returns one row per split-split-plot.
.ssp_layout <- function(plots, f2, f3, col.1, col.2, graph.col) {
  np <- length(plots)
  cols <- .palette(f2, col.1, col.2, graph.col)
  nr <- f2 * f3
  F2 <- matrix(0L, nrow = nr, ncol = np)
  F3 <- matrix(0L, nrow = nr, ncol = np)
  for (p in seq_len(np)) {
    sub <- sample(seq_len(f2))
    F2[, p] <- base::rep(sub, each = f3)
    F3[, p] <- as.vector(vapply(seq_len(f2), function(j) sample(seq_len(f3)),
                                integer(f3)))
  }
  f1lab <- paste0("F1-", plots)
  lbl <- matrix(paste0(base::rep(f1lab, each = nr), "\nF2-", as.vector(F2),
                       "\nF3-", as.vector(F3)), nrow = nr, ncol = np)
  colmat <- matrix(cols[as.vector(F2)], nrow = nr, ncol = np)
  book <- data.frame(plot = base::rep(seq_len(np), each = nr),
                     subplot = base::rep(base::rep(seq_len(f2), each = f3),
                                         times = np),
                     subsubplot = base::rep(seq_len(f3), times = f2 * np),
                     factor1 = base::rep(f1lab, each = nr),
                     factor2 = paste0("F2-", as.vector(F2)),
                     factor3 = paste0("F3-", as.vector(F3)))
  list(lbl = lbl, colmat = colmat, book = book,
       sep.rows = if (f2 > 1) seq_len(f2 - 1) * f3 else NULL)
}

#' @rdname designs3
#' @export
des.ssp.crd <- function(factor1, factor2, factor3, rep, col.1 = "gray",
                        col.2 = "white", graph.col = TRUE,
                        fac.names = c("F1", "F2", "F3"), save = FALSE,
                        file = "des.ssp.crd.tiff", graph = TRUE) {
  .check_design_args(factor1, factor2, factor3, rep)
  plots <- sample(base::rep(seq_len(factor1), rep))
  L <- .ssp_layout(plots, factor2, factor3, col.1, col.2, graph.col)
  mat <- matrix(gsub("\n", " | ", L$lbl), nrow = nrow(L$lbl))
  colnames(mat) <- paste0("Plot-", seq_along(plots))
  if (isTRUE(graph)) {
    .draw_grid(L$lbl, L$colmat,
               main = .fac3_title("Split-split-plot in completely randomized design",
                                  fac.names),
               col.labels = paste0("P", seq_along(plots)), sep.rows = L$sep.rows)
    .save_tiff(save, file, 120 * length(plots), 90 * nrow(L$lbl))
  }
  book <- L$book
  book$rep <- base::rep(.rep_index(plots), each = factor2 * factor3)
  print(mat)
  invisible(list(Design = mat, Book = book))
}

#' @rdname designs3
#' @export
des.ssp.rbd <- function(factor1, factor2, factor3, rep, col.1 = "gray",
                        col.2 = "white", graph.col = TRUE,
                        fac.names = c("F1", "F2", "F3"), save = FALSE,
                        file = "des.ssp.rbd.tiff", graph = TRUE) {
  .check_design_args(factor1, factor2, factor3, rep)
  r <- rep
  plots <- as.vector(vapply(seq_len(r), function(b) sample(seq_len(factor1)),
                            integer(factor1)))
  block <- base::rep(seq_len(r), each = factor1)
  L <- .ssp_layout(plots, factor2, factor3, col.1, col.2, graph.col)
  mat <- matrix(gsub("\n", " | ", L$lbl), nrow = nrow(L$lbl))
  colnames(mat) <- paste0("Block-", block, ".Plot-",
                          base::rep(seq_len(factor1), times = r))
  if (isTRUE(graph)) {
    .draw_grid(L$lbl, L$colmat,
               main = .fac3_title("Split-split-plot in randomized block design",
                                  fac.names),
               col.labels = paste0("B", block),
               sep.cols = if (r > 1) seq_len(r - 1) * factor1 else NULL,
               sep.rows = L$sep.rows)
    .save_tiff(save, file, 120 * length(plots), 90 * nrow(L$lbl))
  }
  book <- cbind(Block = base::rep(block, each = factor2 * factor3), L$book)
  print(mat)
  invisible(list(Design = mat, Book = book))
}
