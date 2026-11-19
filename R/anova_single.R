#' Analysis of variance: one factor experiments
#'
#' Analysis of variance of balanced experiments with a single factor in the
#' completely randomized design (\code{anova_crd}), randomized block design
#' (\code{anova_rbd}) or Latin square design (\code{latsd}), considering a
#' fixed model. The output contains the ANOVA table, the coefficient of
#' variation, the Shapiro-Wilk test of normality of the residuals and the
#' multiple comparison test (qualitative treatments) or the polynomial
#' regression (quantitative treatments).
#'
#' Multiple comparisons (or regressions) are always performed; when the F
#' test of treatments is not significant a note is printed.
#'
#' @param treat Vector with the treatments.
#' @param resp Numeric vector with the response variable.
#' @param block Vector with the blocks (\code{anova_rbd}).
#' @param row,column Vectors with the rows and columns (\code{latsd}).
#' @param quali Logical. If \code{TRUE} (default), the treatments are
#'   qualitative and a multiple comparison test is applied; if \code{FALSE},
#'   they are quantitative and a polynomial regression is fitted
#'   (\code{\link{reg.poly}}).
#' @param graph Logical. If \code{TRUE} (default), graphs are drawn.
#' @param graph.type Type of graph of the multiple comparison test:
#'   \code{"barplotb"}, \code{"barplotg"}, \code{"pointplotg"} or
#'   \code{"boxplotb"}.
#' @param graph.col Color used in the graphs.
#' @param xlab,ylab Axis labels.
#' @param main Title of the graph.
#' @param graph.den Density of the shading lines of the bars
#'   (\code{"barplotb"}); \code{NULL} for solid bars.
#' @param mcomp Multiple comparison test: \code{"tukey"} (default),
#'   \code{"duncan"}, \code{"lsd"}, \code{"snk"}, \code{"scottknott"} (or
#'   \code{"sk"}) or \code{"scheffe"}.
#' @param best.fit Logical. Passed to \code{\link{reg.poly}}: if \code{TRUE}
#'   (default), only the best regression model is drawn.
#' @param sigF Significance level of the F test and of the multiple
#'   comparison test (default 0.05).
#' @param sigT Significance level of the multiple comparison test
#'   (\code{latsd} only; default 0.05).
#' @return Invisibly, a list with the elements \code{Anova} (ANOVA table),
#'   \code{Shapiro.test} (p-value), \code{Shapiro.result} (message),
#'   \code{CV} (coefficient of variation) and \code{Mcomp} (result of the
#'   multiple comparison test, see \code{\link{tukey}}) or \code{Reg}
#'   (result of \code{\link{reg.poly}}).
#' @references Banzatto, D. A.; Kronka, S. N. Experimentacao Agricola.
#'   4. ed. Jaboticabal: Funep, 2006.
#'
#'   Gomes, F. P. Curso de Estatistica Experimental. 10. ed. Piracicaba:
#'   ESALQ/USP, 1982.
#' @author Cicero Carlos de Souza Almeida. Based on ExpDes (E. B. Ferreira,
#'   D. A. Nogueira, P. P. Cavalcanti).
#' @seealso \code{\link{tukey}}, \code{\link{reg.poly}},
#'   \code{\link{anova_fat2_crd}}, \code{\link{anova_sp2_crd}}
#' @examples
#' # Completely randomized design
#' crd <- read.csv(system.file("extdata", "crd.csv", package = "AgriR"))
#' res <- anova_crd(crd$trat, crd$resp, mcomp = "tukey")
#' res$Anova
#'
#' # Randomized block design, Scott-Knott test, ggplot2 graph
#' rbd <- read.csv(system.file("extdata", "rbd.csv", package = "AgriR"))
#' anova_rbd(rbd$trat, rbd$resp, rbd$bloco, mcomp = "sk",
#'           graph.type = "barplotg")
#'
#' # Quantitative treatments: polynomial regression
#' dose <- read.csv(system.file("extdata", "dose.csv", package = "AgriR"))
#' anova_crd(dose$dose, dose$resp, quali = FALSE, xlab = "Dose (kg/ha)",
#'           ylab = "Yield")
#'
#' # Latin square
#' lq <- read.csv(system.file("extdata", "latsq.csv", package = "AgriR"))
#' latsd(lq$trat, lq$linha, lq$coluna, lq$resp, mcomp = "duncan")
#' @name anova_single
NULL

.single_factor <- function(model, s, treat.row, err.row, rows, labels,
                           treat, resp, quali, graph, graph.type, graph.col,
                           xlab, ylab, main, graph.den, mcomp, best.fit, sigF,
                           sigT = sigF) {
  tab <- .anova_out(s, rows, labels)
  .print_anova(tab)
  cv <- .cv_table(s[err.row, "MS"], resp)
  .print_cv(cv)
  sh <- .shapiro(stats::residuals(model))
  .print_shapiro(sh)
  .f_note(s[treat.row, "Pr>Fc"], sigF)
  dfE <- s[err.row, "DF"]
  ssE <- s[err.row, "SS"]
  res <- .analyze_factor(resp, treat, quali, dfE, ssE, s[treat.row, "DF"],
                         s[treat.row, "SS"], mcomp, sigT, graph, graph.type,
                         graph.col, graph.den, xlab, ylab, main, best.fit)
  out <- list(Anova = tab, Shapiro.test = sh$p, Shapiro.result = sh$msg,
              CV = cv)
  if (quali) out$Mcomp <- res else out$Reg <- res
  invisible(out)
}

#' @rdname anova_single
#' @export
anova_crd <- function(treat, resp, quali = TRUE, graph = TRUE,
                      graph.type = "barplotb", graph.col = "skyblue",
                      xlab = "Treatments", ylab = "Variable", main = "",
                      graph.den = 2, mcomp = "tukey", best.fit = TRUE,
                      sigF = 0.05) {
  .check_resp(resp, treat)
  if (quali) mcomp <- .match_mcomp(mcomp)
  if (graph) graph.type <- .match_graph_type(graph.type)
  Trat <- factor(treat)
  model <- stats::aov(resp ~ Trat)
  s <- .aov_terms(model)
  .single_factor(model, s, "Trat", "Residuals", c("Trat", "Residuals"),
                 c("Treatments", "Residuals"), treat, resp, quali, graph,
                 graph.type, graph.col, xlab, ylab, main, graph.den, mcomp,
                 best.fit, sigF)
}

#' @rdname anova_single
#' @export
anova_rbd <- function(treat, resp, block, quali = TRUE, graph = TRUE,
                      graph.type = "barplotb", graph.col = "skyblue",
                      xlab = "Treatments", ylab = "Variable", main = "",
                      graph.den = 2, mcomp = "tukey", best.fit = TRUE,
                      sigF = 0.05) {
  .check_resp(resp, treat, block)
  if (quali) mcomp <- .match_mcomp(mcomp)
  if (graph) graph.type <- .match_graph_type(graph.type)
  Trat <- factor(treat)
  Bloco <- factor(block)
  model <- stats::aov(resp ~ Trat + Bloco)
  s <- .aov_terms(model)
  .single_factor(model, s, "Trat", "Residuals", c("Trat", "Bloco", "Residuals"),
                 c("Treatments", "Blocks", "Residuals"), treat, resp, quali,
                 graph, graph.type, graph.col, xlab, ylab, main, graph.den,
                 mcomp, best.fit, sigF)
}

#' @rdname anova_single
#' @export
latsd <- function(treat, row, column, resp, quali = TRUE, mcomp = "tukey",
                  sigT = 0.05, sigF = 0.05, graph = TRUE,
                  graph.type = "barplotb", graph.col = "skyblue",
                  xlab = "Treatments", ylab = "Variable", main = "",
                  graph.den = 2, best.fit = TRUE) {
  .check_resp(resp, treat, row, column)
  if (quali) mcomp <- .match_mcomp(mcomp)
  if (graph) graph.type <- .match_graph_type(graph.type)
  Trat <- factor(treat)
  Linha <- factor(row)
  Coluna <- factor(column)
  model <- stats::aov(resp ~ Trat + Linha + Coluna)
  s <- .aov_terms(model)
  .single_factor(model, s, "Trat", "Residuals",
                 c("Trat", "Linha", "Coluna", "Residuals"),
                 c("Treatments", "Rows", "Columns", "Residuals"), treat, resp,
                 quali, graph, graph.type, graph.col, xlab, ylab, main,
                 graph.den, mcomp, best.fit, sigF, sigT)
}
