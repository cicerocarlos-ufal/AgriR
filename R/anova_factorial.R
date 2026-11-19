#' Analysis of variance: double factorial scheme
#'
#' Analysis of variance of balanced experiments in a double factorial scheme
#' in the completely randomized design (\code{anova_fat2_crd}) or randomized
#' block design (\code{anova_fat2_rbd}), considering a fixed model.
#'
#' When the interaction is not significant, the main effect of each factor
#' is analysed (multiple comparison test for qualitative factors, polynomial
#' regression for quantitative ones). When the interaction is significant,
#' it is unfolded: factor 1 is analysed inside each level of factor 2
#' (\code{fac.fix = "fac.A"}) or factor 2 inside each level of factor 1
#' (\code{fac.fix = "fac.B"}), always with the error of the experiment.
#'
#' @param factor1,factor2 Vectors with the levels of factors 1 and 2.
#' @param block Vector with the blocks (\code{anova_fat2_rbd}).
#' @param resp Numeric vector with the response variable.
#' @param quali Logical vector of length 2: \code{TRUE} for a qualitative
#'   factor, \code{FALSE} for a quantitative factor.
#' @param fac.names Names of factors 1 and 2, used in the output.
#' @param fac.fix How a significant interaction is unfolded: \code{"fac.A"}
#'   (factor 1 inside the levels of factor 2, default) or \code{"fac.B"}
#'   (factor 2 inside the levels of factor 1).
#' @inheritParams anova_single
#' @return Invisibly, a list with \code{Anova}, \code{Shapiro.test},
#'   \code{Shapiro.result}, \code{CV}, \code{Unfolding} (ANOVA of the
#'   unfolding of the interaction, when it is significant) and the named
#'   lists \code{Mcomp} (multiple comparison tests, see \code{\link{tukey}})
#'   and/or \code{Reg} (regressions, see \code{\link{reg.poly}}).
#' @references Banzatto, D. A.; Kronka, S. N. Experimentacao Agricola.
#'   4. ed. Jaboticabal: Funep, 2006.
#' @author Cicero Carlos de Souza Almeida. Based on ExpDes (E. B. Ferreira,
#'   D. A. Nogueira, P. P. Cavalcanti).
#' @seealso \code{\link{anova_crd}}, \code{\link{anova_sp2_crd}}
#' @examples
#' fat <- read.csv(system.file("extdata", "fat2.csv", package = "AgriR"))
#' anova_fat2_crd(fat$cultivar, fat$adubo, fat$resp,
#'                fac.names = c("Cultivar", "Fertilizer"))
#' anova_fat2_rbd(fat$cultivar, fat$adubo, fat$bloco, fat$resp,
#'                fac.names = c("Cultivar", "Fertilizer"), fac.fix = "fac.B",
#'                graph = FALSE)
#' @name anova_fat2
NULL

.fat2 <- function(factor1, factor2, resp, block, quali, fac.names, fac.fix,
                  graph, graph.type, graph.col, xlab, ylab, main, graph.den,
                  mcomp, best.fit, sigF) {
  quali <- rep_len(as.logical(quali), 2)
  fac.names <- rep_len(as.character(fac.names), 2)
  if (any(quali)) mcomp <- .match_mcomp(mcomp)
  if (graph) graph.type <- .match_graph_type(graph.type)
  fac.fix <- .match_fac_fix(fac.fix)
  rbd <- !is.null(block)

  cat(.line(), "\nLegend:\n", sep = "")
  cat("FACTOR 1: ", fac.names[1], "\n", sep = "")
  cat("FACTOR 2: ", fac.names[2], "\n", .line(), "\n", sep = "")

  Fator1 <- factor(factor1)
  Fator2 <- factor(factor2)
  if (rbd) {
    Bloco <- factor(block)
    model <- stats::aov(resp ~ Bloco + Fator1 * Fator2)
  } else {
    model <- stats::aov(resp ~ Fator1 * Fator2)
  }
  s <- .aov_terms(model)
  r.int <- .find_row(s, c("Fator1", "Fator2"))
  rows <- c(if (rbd) "Bloco", "Fator1", "Fator2", r.int, "Residuals")
  labels <- c(if (rbd) "Block", fac.names[1], fac.names[2],
              paste0(fac.names[1], "*", fac.names[2]), "Residuals")
  tab <- .anova_out(s, rows, labels)
  .print_anova(tab)
  cv <- .cv_table(s["Residuals", "MS"], resp)
  .print_cv(cv)
  sh <- .shapiro(stats::residuals(model))
  .print_shapiro(sh)

  dfE <- s["Residuals", "DF"]
  ssE <- s["Residuals", "SS"]
  MSE <- ssE / dfE
  pInt <- s[r.int, "Pr>Fc"]
  fatores <- list(factor1, factor2)
  Fs <- list(Fator1, Fator2)
  frows <- c("Fator1", "Fator2")
  Mcomp <- list()
  Reg <- list()
  Unfolding <- NULL

  store <- function(res, i, key) {
    if (quali[i]) Mcomp[[key]] <<- res else Reg[[key]] <<- res
  }

  if (is.na(pInt) || pInt > sigF) {
    cat("\nNo significant interaction: analyzing the main effects\n",
        .line(), "\n", sep = "")
    op <- .set_layout(2, graph, graph.type)
    if (!is.null(op)) on.exit(graphics::par(op), add = TRUE)
    for (i in 1:2) {
      cat("\n", fac.names[i], "\n", sep = "")
      .f_note(s[frows[i], "Pr>Fc"], sigF, paste("the means of", fac.names[i]))
      res <- .analyze_factor(resp, fatores[[i]], quali[i], dfE, ssE,
                             s[frows[i], "DF"], s[frows[i], "SS"], mcomp,
                             sigF, graph, graph.type, graph.col, graph.den,
                             xlab, ylab, fac.names[i], best.fit)
      store(res, i, fac.names[i])
    }
  } else {
    cat("\nSignificant interaction: analyzing the interaction\n", .line(),
        "\n", sep = "")
    inner <- if (fac.fix == "fac.A") 1 else 2
    outer <- 3 - inner
    cat("\nAnalyzing ", fac.names[inner], " inside of each level of ",
        fac.names[outer], "\n", sep = "")
    levs <- levels(Fs[[outer]])
    ok <- !is.na(resp)
    ss.in <- vapply(levs, function(l) {
      m <- Fs[[outer]] == l
      .ss_oneway(resp[m], Fs[[inner]][m])
    }, numeric(1))
    df.in <- vapply(levs, function(l) {
      m <- Fs[[outer]] == l & ok
      length(unique(Fs[[inner]][m])) - 1
    }, numeric(1))
    DF <- c(if (rbd) s["Bloco", "DF"], s[frows[outer], "DF"], df.in, dfE,
            sum(s$DF))
    SS <- c(if (rbd) s["Bloco", "SS"], s[frows[outer], "SS"], ss.in, ssE,
            sum(s$SS))
    MS <- SS / DF
    MS[length(MS)] <- NA
    Fc <- MS / MSE
    Fc[(length(Fc) - 1):length(Fc)] <- NA
    P <- stats::pf(Fc, DF, dfE, lower.tail = FALSE)
    Unfolding <- data.frame(DF = DF, SS = round(SS, 5), MS = round(MS, 5),
                            Fc = round(Fc, 4), "Pr>Fc" = round(P, 4),
                            check.names = FALSE)
    rownames(Unfolding) <- c(if (rbd) "Block", fac.names[outer],
                             paste0(fac.names[inner], " : ", fac.names[outer],
                                    " ", levs),
                             "Residuals", "Total")
    .print_anova(Unfolding, "Analysis of Variance Table (unfolding)")

    op <- .set_layout(length(levs), graph, graph.type)
    if (!is.null(op)) on.exit(graphics::par(op), add = TRUE)
    for (j in seq_along(levs)) {
      m <- Fs[[outer]] == levs[j]
      titulo <- paste(fac.names[inner], "inside of the level", levs[j], "of",
                      fac.names[outer])
      cat("\n", titulo, "\n", sep = "")
      res <- .analyze_factor(resp[m], fatores[[inner]][m], quali[inner], dfE,
                             ssE, df.in[j], ss.in[j], mcomp, sigF, graph,
                             graph.type, graph.col, graph.den, xlab, ylab,
                             titulo, best.fit)
      store(res, inner, paste0(fac.names[outer], " = ", levs[j]))
    }
  }

  out <- list(Anova = tab, Shapiro.test = sh$p, Shapiro.result = sh$msg,
              CV = cv)
  if (!is.null(Unfolding)) out$Unfolding <- Unfolding
  if (length(Mcomp)) out$Mcomp <- Mcomp
  if (length(Reg)) out$Reg <- Reg
  invisible(out)
}

#' @rdname anova_fat2
#' @export
anova_fat2_crd <- function(factor1, factor2, resp, quali = c(TRUE, TRUE),
                           fac.names = c("F1", "F2"), fac.fix = "fac.A",
                           graph = TRUE, graph.type = "barplotb",
                           graph.col = "skyblue", xlab = "Treatments",
                           ylab = "Variable", main = "", graph.den = 2,
                           mcomp = "tukey", best.fit = TRUE, sigF = 0.05) {
  .check_resp(resp, factor1, factor2)
  .fat2(factor1, factor2, resp, NULL, quali, fac.names, fac.fix, graph,
        graph.type, graph.col, xlab, ylab, main, graph.den, mcomp, best.fit,
        sigF)
}

#' @rdname anova_fat2
#' @export
anova_fat2_rbd <- function(factor1, factor2, block, resp, quali = c(TRUE, TRUE),
                           fac.names = c("F1", "F2"), fac.fix = "fac.A",
                           graph = TRUE, graph.type = "barplotb",
                           graph.col = "skyblue", xlab = "Treatments",
                           ylab = "Variable", main = "", graph.den = 2,
                           mcomp = "tukey", best.fit = TRUE, sigF = 0.05) {
  .check_resp(resp, factor1, factor2, block)
  .fat2(factor1, factor2, resp, block, quali, fac.names, fac.fix, graph,
        graph.type, graph.col, xlab, ylab, main, graph.den, mcomp, best.fit,
        sigF)
}
