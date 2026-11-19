#' Analysis of variance: split-plot scheme
#'
#' Analysis of variance of balanced experiments in the split-plot scheme in
#' the completely randomized design (\code{anova_sp2_crd}) or randomized
#' block design (\code{anova_sp2_rbd}), considering a fixed model. Factor 1
#' is in the plots (tested against error a) and factor 2 in the split-plots
#' (tested against error b).
#'
#' When the interaction is significant it is unfolded. Factor 2 inside each
#' level of factor 1 (\code{fac.fix = "fac.B"}) uses error b. Factor 1
#' inside each level of factor 2 (\code{fac.fix = "fac.A"}) uses the pooled
#' error \eqn{(QMa + (b - 1) QMb) / b}, with degrees of freedom given by
#' Satterthwaite's formula.
#'
#' @param factor1 Vector with the levels of factor 1 (plots).
#' @param factor2 Vector with the levels of factor 2 (split-plots).
#' @param repet Vector with the repetitions (\code{anova_sp2_crd}). It
#'   identifies the plots: the plots of the same level of factor 1 must have
#'   different repetition numbers.
#' @param block Vector with the blocks (\code{anova_sp2_rbd}).
#' @param resp Numeric vector with the response variable.
#' @param quali Logical vector of length 2: \code{TRUE} for a qualitative
#'   factor, \code{FALSE} for a quantitative factor.
#' @param fac.fix How a significant interaction is unfolded: \code{"fac.A"}
#'   (factor 1 inside the levels of factor 2, default) or \code{"fac.B"}
#'   (factor 2 inside the levels of factor 1).
#' @param fac.names Names of factors 1 and 2, used in the output.
#' @param main Title of the graphs (not used: the graphs receive the names
#'   of the factors); kept for compatibility.
#' @param sigT Significance level of the multiple comparison tests. By
#'   default, the same as \code{sigF}.
#' @param ... Ignored (kept for compatibility with old arguments such as
#'   \code{nl} and \code{hvar}).
#' @inheritParams anova_single
#' @return Invisibly, a list with \code{Anova}, \code{Shapiro.test},
#'   \code{Shapiro.result}, \code{CV} (plot and split-plot),
#'   \code{Unfolding} (when the interaction is significant) and the named
#'   lists \code{Mcomp} and/or \code{Reg}.
#' @references Banzatto, D. A.; Kronka, S. N. Experimentacao Agricola.
#'   4. ed. Jaboticabal: Funep, 2006.
#' @author Cicero Carlos de Souza Almeida. Based on ExpDes (E. B. Ferreira,
#'   D. A. Nogueira, P. P. Cavalcanti).
#' @seealso \code{\link{anova_fat2_crd}}, \code{\link{des.sp.crd}}
#' @examples
#' sp <- read.csv(system.file("extdata", "split.csv", package = "AgriR"))
#' anova_sp2_rbd(sp$irrigacao, sp$cultivar, sp$bloco, sp$resp,
#'               fac.names = c("Irrigation", "Cultivar"))
#' anova_sp2_crd(sp$irrigacao, sp$cultivar, sp$bloco, sp$resp,
#'               fac.names = c("Irrigation", "Cultivar"), fac.fix = "fac.B",
#'               graph = FALSE)
#' @name anova_sp2
NULL

.split2 <- function(factor1, factor2, R, resp, rbd, quali, fac.fix, fac.names,
                    graph, graph.type, graph.col, xlab, ylab, graph.den,
                    mcomp, best.fit, sigF, sigT) {
  quali <- rep_len(as.logical(quali), 2)
  fac.names <- rep_len(as.character(fac.names), 2)
  if (any(quali)) mcomp <- .match_mcomp(mcomp)
  if (graph) graph.type <- .match_graph_type(graph.type)
  fac.fix <- .match_fac_fix(fac.fix)

  cat(.line(), "\nLegend:\n", sep = "")
  cat("FACTOR 1 (plot): ", fac.names[1], "\n", sep = "")
  cat("FACTOR 2 (split-plot): ", fac.names[2], "\n", .line(), "\n", sep = "")

  Fator1 <- factor(factor1)
  Fator2 <- factor(factor2)
  R <- factor(R)
  if (rbd) {
    model <- stats::aov(resp ~ R + Fator1 * Fator2 + R:Fator1)
  } else {
    model <- stats::aov(resp ~ Fator1 * Fator2 + Fator1:R)
  }
  s <- .aov_terms(model)
  r.int <- .find_row(s, c("Fator1", "Fator2"))
  r.ea <- .find_row(s, c("Fator1", "R"))

  dfa <- s[r.ea, "DF"]
  MSa <- s[r.ea, "MS"]
  dfb <- s["Residuals", "DF"]
  MSb <- s["Residuals", "MS"]

  # Factor 1 (and blocks) are tested against error a.
  s["Fator1", "Fc"] <- s["Fator1", "MS"] / MSa
  s["Fator1", "Pr>Fc"] <- stats::pf(s["Fator1", "Fc"], s["Fator1", "DF"], dfa,
                                    lower.tail = FALSE)
  if (rbd) {
    s["R", "Fc"] <- s["R", "MS"] / MSa
    s["R", "Pr>Fc"] <- stats::pf(s["R", "Fc"], s["R", "DF"], dfa,
                                 lower.tail = FALSE)
  }
  s[r.ea, c("Fc", "Pr>Fc")] <- NA

  rows <- c(if (rbd) "R", "Fator1", r.ea, "Fator2", r.int, "Residuals")
  labels <- c(if (rbd) "Block", fac.names[1], "Error a", fac.names[2],
              paste0(fac.names[1], "*", fac.names[2]), "Error b")
  tab <- .anova_out(s, rows, labels)
  .print_anova(tab)
  cv <- list(Plot = .cv_table(MSa, resp, "Coefficient of Variance (Plot)"),
             SplitPlot = .cv_table(MSb, resp,
                                   "Coefficient of Variance (SplitPlot)"))
  .print_cv(cv$Plot)
  .print_cv(cv$SplitPlot)
  sh <- .shapiro(stats::residuals(model))
  .print_shapiro(sh)

  fatores <- list(factor1, factor2)
  Fs <- list(Fator1, Fator2)
  frows <- c("Fator1", "Fator2")
  err <- list(c(dfa, dfa * MSa), c(dfb, dfb * MSb))
  pInt <- s[r.int, "Pr>Fc"]
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
      res <- .analyze_factor(resp, fatores[[i]], quali[i], err[[i]][1],
                             err[[i]][2], s[frows[i], "DF"], s[frows[i], "SS"],
                             mcomp, sigT, graph, graph.type, graph.col,
                             graph.den, xlab, ylab, fac.names[i], best.fit)
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
    if (inner == 1) {
      # Factor 1 inside factor 2: pooled error with Satterthwaite's df.
      b <- nlevels(Fator2)
      MSc <- (MSa + (b - 1) * MSb) / b
      dfc <- (MSa + (b - 1) * MSb)^2 /
        (MSa^2 / dfa + ((b - 1) * MSb)^2 / dfb)
      err.name <- "Pooled error"
    } else {
      MSc <- MSb
      dfc <- dfb
      err.name <- "Error b"
    }
    Fc <- (ss.in / df.in) / MSc
    P <- stats::pf(Fc, df.in, dfc, lower.tail = FALSE)
    Unfolding <- data.frame(DF = round(c(df.in, dfc), 2),
                            SS = round(c(ss.in, MSc * dfc), 5),
                            MS = round(c(ss.in / df.in, MSc), 5),
                            Fc = round(c(Fc, NA), 4),
                            "Pr>Fc" = round(c(P, NA), 4),
                            check.names = FALSE)
    rownames(Unfolding) <- c(paste0(fac.names[inner], " : ", fac.names[outer],
                                    " ", levs), err.name)
    .print_anova(Unfolding, "Analysis of Variance Table (unfolding)")

    op <- .set_layout(length(levs), graph, graph.type)
    if (!is.null(op)) on.exit(graphics::par(op), add = TRUE)
    for (j in seq_along(levs)) {
      m <- Fs[[outer]] == levs[j]
      titulo <- paste(fac.names[inner], "inside of the level", levs[j], "of",
                      fac.names[outer])
      cat("\n", titulo, "\n", sep = "")
      res <- .analyze_factor(resp[m], fatores[[inner]][m], quali[inner], dfc,
                             MSc * dfc, df.in[j], ss.in[j], mcomp, sigT,
                             graph, graph.type, graph.col, graph.den, xlab,
                             ylab, titulo, best.fit)
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

#' @rdname anova_sp2
#' @export
anova_sp2_crd <- function(factor1, factor2, repet, resp, quali = c(TRUE, TRUE),
                          fac.fix = "fac.A", fac.names = c("fac.A", "fac.B"),
                          graph = TRUE, graph.type = "barplotb",
                          graph.col = "skyblue", xlab = "Treatments",
                          ylab = "Variable", graph.den = 2, mcomp = "tukey",
                          sigF = 0.05, best.fit = TRUE, main = "",
                          sigT = sigF, ...) {
  .check_resp(resp, factor1, factor2, repet)
  .split2(factor1, factor2, repet, resp, FALSE, quali, fac.fix, fac.names,
          graph, graph.type, graph.col, xlab, ylab, graph.den, mcomp,
          best.fit, sigF, sigT)
}

#' @rdname anova_sp2
#' @export
anova_sp2_rbd <- function(factor1, factor2, block, resp, quali = c(TRUE, TRUE),
                          fac.fix = "fac.A", fac.names = c("fac.A", "fac.B"),
                          graph = TRUE, graph.type = "barplotb",
                          graph.col = "skyblue", xlab = "Treatments",
                          ylab = "Variable", graph.den = 2, mcomp = "tukey",
                          sigF = 0.05, best.fit = TRUE, main = "",
                          sigT = sigF, ...) {
  .check_resp(resp, factor1, factor2, block)
  .split2(factor1, factor2, block, resp, TRUE, quali, fac.fix, fac.names,
          graph, graph.type, graph.col, xlab, ylab, graph.den, mcomp,
          best.fit, sigF, sigT)
}
