#' Multiple comparison tests
#'
#' Tukey, Duncan, LSD (t test), Student-Newman-Keuls (SNK), Scott-Knott and
#' Scheffe tests for the comparison of treatment means, using the error
#' degrees of freedom and sum of squares of a previous analysis of variance.
#' The means are grouped with letters (means followed by the same letter do
#' not differ) and, optionally, a graph is drawn.
#'
#' These functions are called by the ANOVA functions of the package
#' (\code{\link{anova_crd}}, \code{\link{anova_rbd}}, \code{\link{latsd}},
#' \code{\link{anova_fat2_crd}}, ...), but can also be used directly.
#'
#' The Scott-Knott test follows Scott & Knott (1974): the ordered means are
#' split into the two groups that maximize the between-groups sum of squares,
#' and the split is accepted when the likelihood ratio statistic is
#' significant; the procedure is repeated inside each new group until no
#' split is significant.
#'
#' @param y Numeric vector with the response variable.
#' @param trt Vector with the treatments (same length as \code{y}).
#' @param DFerror Degrees of freedom of the error.
#' @param SSerror Sum of squares of the error.
#' @param alpha Significance level (default 0.05).
#' @param graph Logical. If \code{TRUE} (default), a graph is drawn.
#' @param graph.col Color of the bars, boxes or points.
#' @param graph.den Density of the shading lines of the bars
#'   (\code{graph.type = "barplotb"} only); \code{NULL} for solid bars.
#' @param graph.type Type of graph: \code{"barplotb"} (base bar plot),
#'   \code{"barplotg"} (ggplot2 bar plot), \code{"pointplotg"} (ggplot2 point
#'   plot) or \code{"boxplotb"} (base box plot).
#' @param xlab,ylab Axis labels.
#' @param group Logical. Only the grouping of means is implemented; kept for
#'   compatibility.
#' @param main Title of the graph.
#' @return Invisibly, a data frame (one row per treatment, in the order of
#'   the levels of \code{trt}) with the columns \code{Medias} (means),
#'   \code{sd} (standard deviation), \code{se.sup} and \code{se.inf}
#'   (mean plus and minus one standard deviation, used for the error bars)
#'   and a fifth column with the letters of the test (\code{tukey},
#'   \code{duncan}, \code{lsd}, \code{snk}, \code{scott} or \code{scheffe}).
#' @references
#' Banzatto, D. A.; Kronka, S. N. Experimentacao Agricola. 4. ed.
#' Jaboticabal: Funep, 2006.
#'
#' Scott, A. J.; Knott, M. A cluster analysis method for grouping means in
#' the analysis of variance. Biometrics, v. 30, p. 507-512, 1974.
#'
#' Steel, R. G. D.; Torrie, J. H.; Dickey, D. A. Principles and Procedures
#' of Statistics: a Biometrical Approach. 3. ed. McGraw-Hill, 1997.
#' @author Cicero Carlos de Souza Almeida. Grouping algorithms adapted from
#'   the ExpDes (E. B. Ferreira, D. A. Nogueira, P. P. Cavalcanti) and
#'   agricolae (F. de Mendiburu) packages.
#' @seealso \code{\link{anova_crd}}, \code{\link{barplotb}}
#' @examples
#' y <- c(25, 26, 24, 27, 30, 31, 29, 32, 35, 36, 34, 37)
#' trt <- rep(c("A", "B", "C"), each = 4)
#' fit <- aov(y ~ factor(trt))
#' DFe <- fit$df.residual
#' SSe <- sum(residuals(fit)^2)
#' tukey(y, trt, DFe, SSe)
#' duncan(y, trt, DFe, SSe, graph = FALSE)
#' lsd(y, trt, DFe, SSe, graph.type = "boxplotb")
#' snk(y, trt, DFe, SSe, graph = FALSE)
#' scottknott(y, trt, DFe, SSe, graph = FALSE)
#' scheffe(y, trt, DFe, SSe, graph = FALSE)
#' @name mcomp
NULL

.finish_mcomp <- function(d, groups, colname, graph, graph.type, graph.col,
                          graph.den, xlab, ylab, main) {
  tg <- .table_graph(d, groups, colname)
  .plot_mcomp(tg, d, graph, graph.type, graph.col, graph.den, xlab, ylab, main)
  invisible(tg)
}

.check_group <- function(group) {
  if (!isTRUE(group)) {
    warning("only the grouping of means (group = TRUE) is implemented",
            call. = FALSE)
  }
}

#' @rdname mcomp
#' @export
tukey <- function(y, trt, DFerror, SSerror, alpha = 0.05, graph = TRUE,
                  graph.col = "skyblue", graph.den = NULL,
                  graph.type = "barplotb", xlab = "", ylab = "", group = TRUE,
                  main = "") {
  .check_group(group)
  d <- .prep_mcomp(y, trt)
  MSerror <- SSerror / DFerror
  Tprob <- stats::qtukey(1 - alpha, length(d$means), DFerror)
  out <- order.group(d$lev, d$means, d$n, MSerror, Tprob, d$sds / sqrt(d$n),
                     parameter = 0.5)
  cat("\nTukey's test\n", .line(), "\n", sep = "")
  .print_groups(out$trt, out$means, out$M)
  cat(.line(), "\n", sep = "")
  .finish_mcomp(d, setNames(out$M, out$trt), "tukey", graph, graph.type,
                graph.col, graph.den, xlab, ylab, main)
}

#' @rdname mcomp
#' @export
lsd <- function(y, trt, DFerror, SSerror, alpha = 0.05, graph = TRUE,
                graph.col = "skyblue", graph.den = NULL,
                graph.type = "barplotb", xlab = "", ylab = "", group = TRUE,
                main = "") {
  .check_group(group)
  d <- .prep_mcomp(y, trt)
  MSerror <- SSerror / DFerror
  Tprob <- stats::qt(1 - alpha / 2, DFerror) * sqrt(2)
  out <- order.group(d$lev, d$means, d$n, MSerror, Tprob, d$sds / sqrt(d$n),
                     parameter = 0.5)
  cat("\nT test (LSD)\n", .line(), "\n", sep = "")
  .print_groups(out$trt, out$means, out$M)
  cat(.line(), "\n", sep = "")
  .finish_mcomp(d, setNames(out$M, out$trt), "lsd", graph, graph.type,
                graph.col, graph.den, xlab, ylab, main)
}

# Shared by duncan() and snk(): critical ranges for 2, 3, ..., k means.
.range_test <- function(d, MSerror, Tprob) {
  nr <- 1 / mean(1 / d$n)
  HSD <- Tprob * sqrt(MSerror / nr)
  order.stat.SNK(d$lev, d$means, HSD)
}

#' @rdname mcomp
#' @export
duncan <- function(y, trt, DFerror, SSerror, alpha = 0.05, graph = TRUE,
                   graph.col = "skyblue", graph.den = NULL,
                   graph.type = "barplotb", xlab = "", ylab = "", group = TRUE,
                   main = "") {
  .check_group(group)
  d <- .prep_mcomp(y, trt)
  MSerror <- SSerror / DFerror
  p <- 2:length(d$means)
  Tprob <- stats::qtukey((1 - alpha)^(p - 1), p, DFerror)
  out <- .range_test(d, MSerror, Tprob)
  cat("\nDuncan's test\n", .line(), "\n", sep = "")
  .print_groups(out$trt, out$means, out$M)
  cat(.line(), "\n", sep = "")
  .finish_mcomp(d, setNames(out$M, out$trt), "duncan", graph, graph.type,
                graph.col, graph.den, xlab, ylab, main)
}

#' @rdname mcomp
#' @export
snk <- function(y, trt, DFerror, SSerror, alpha = 0.05, graph = TRUE,
                graph.col = "skyblue", graph.den = NULL,
                graph.type = "barplotb", xlab = "", ylab = "", group = TRUE,
                main = "") {
  .check_group(group)
  d <- .prep_mcomp(y, trt)
  MSerror <- SSerror / DFerror
  p <- 2:length(d$means)
  Tprob <- stats::qtukey(1 - alpha, p, DFerror)
  out <- .range_test(d, MSerror, Tprob)
  cat("\nStudent-Newman-Keuls's test (SNK)\n", .line(), "\n", sep = "")
  .print_groups(out$trt, out$means, out$M)
  cat(.line(), "\n", sep = "")
  .finish_mcomp(d, setNames(out$M, out$trt), "snk", graph, graph.type,
                graph.col, graph.den, xlab, ylab, main)
}

#' @rdname mcomp
#' @export
scheffe <- function(y, trt, DFerror, SSerror, alpha = 0.05, graph = TRUE,
                    graph.col = "skyblue", graph.den = NULL,
                    graph.type = "barplotb", xlab = "", ylab = "",
                    group = TRUE, main = "") {
  .check_group(group)
  d <- .prep_mcomp(y, trt)
  MSerror <- SSerror / DFerror
  ntr <- length(d$means)
  Q <- matrix(1, ntr, ntr)
  for (i in 1:(ntr - 1)) {
    for (j in (i + 1):ntr) {
      dif <- d$means[i] - d$means[j]
      sdtdif2 <- MSerror * (1 / d$n[i] + 1 / d$n[j])
      Fc <- dif^2 / ((ntr - 1) * sdtdif2)
      Q[i, j] <- Q[j, i] <- stats::pf(Fc, ntr - 1, DFerror, lower.tail = FALSE)
    }
  }
  out <- orderPvalue(d$lev, d$means, alpha, Q)
  cat("\nScheffe's test\n", .line(), "\n", sep = "")
  cat("Critical value of F: ", format(stats::qf(1 - alpha, ntr - 1, DFerror),
                                      digits = 5), "\n", sep = "")
  .print_groups(out$trt, out$means, out$M)
  cat(.line(), "\n", sep = "")
  .finish_mcomp(d, setNames(out$M, out$trt), "scheffe", graph, graph.type,
                graph.col, graph.den, xlab, ylab, main)
}

# Recursive Scott-Knott partition. 'm' are means sorted in decreasing order;
# returns an integer group id for each mean.
.sk_split <- function(m, s2, dfr, alpha) {
  k <- length(m)
  ids <- rep(1L, k)
  if (k < 2) return(ids)
  tot <- sum(m)
  b0 <- -Inf
  cut <- 1
  for (i in 1:(k - 1)) {
    g1 <- m[1:i]
    g2 <- m[(i + 1):k]
    B <- sum(g1)^2 / i + sum(g2)^2 / (k - i) - tot^2 / k
    if (B > b0) {
      b0 <- B
      cut <- i
    }
  }
  sigma2 <- (sum((m - mean(m))^2) + dfr * s2) / (k + dfr)
  if (!is.finite(sigma2) || sigma2 <= 0) return(ids)
  lambda <- pi / (2 * (pi - 2)) * b0 / sigma2
  p <- stats::pchisq(lambda, df = k / (pi - 2), lower.tail = FALSE)
  if (p >= alpha) return(ids)
  left <- .sk_split(m[1:cut], s2, dfr, alpha)
  right <- .sk_split(m[(cut + 1):k], s2, dfr, alpha)
  c(left, right + max(left))
}

#' @rdname mcomp
#' @export
scottknott <- function(y, trt, DFerror, SSerror, alpha = 0.05, graph = TRUE,
                       graph.col = "skyblue", graph.den = NULL,
                       graph.type = "barplotb", xlab = "", ylab = "",
                       group = TRUE, main = "") {
  .check_group(group)
  d <- .prep_mcomp(y, trt)
  MSerror <- SSerror / DFerror
  r <- 1 / mean(1 / d$n)
  s2 <- MSerror / r
  ord <- order(d$means, decreasing = TRUE)
  m <- d$means[ord]
  ids <- .sk_split(m, s2, DFerror, alpha)
  lab <- .group_labels(max(ids))[ids]
  cat("\nScott-Knott test\n", .line(), "\n", sep = "")
  .print_groups(d$lev[ord], m, lab)
  cat(.line(), "\n", sep = "")
  .finish_mcomp(d, setNames(lab, d$lev[ord]), "scott", graph, graph.type,
                graph.col, graph.den, xlab, ylab, main)
}
