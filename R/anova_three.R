#' Analysis of variance: experiments with three factors
#'
#' Analysis of variance of balanced experiments with three factors, in a
#' triple factorial scheme (\code{anova_fat3_crd}, \code{anova_fat3_rbd}) or
#' in a split-split-plot scheme (\code{anova_ssp_crd}, \code{anova_ssp_rbd}),
#' in the completely randomized or randomized block design, considering a
#' fixed model.
#'
#' Unfolding of the interactions:
#' \itemize{
#'   \item If the triple interaction is significant, each factor is analysed
#'     inside each combination of the levels of the other two.
#'   \item Otherwise, for each significant double interaction, each of its
#'     two factors is analysed inside each level of the other. A factor that
#'     is not involved in any significant interaction is analysed by its
#'     main effect.
#' }
#' Qualitative factors are analysed with a multiple comparison test and
#' quantitative factors with polynomial regression (\code{\link{reg.poly}}).
#'
#' In the triple factorial all tests use the residual error. In the
#' split-split-plot scheme factor 1 is in the plots (error a), factor 2 in
#' the split-plots (error b) and factor 3 in the split-split-plots
#' (error c). Comparisons that involve more than one error use the pooled
#' error below (\eqn{a}, \eqn{b}, \eqn{c}: numbers of levels of the
#' factors; \eqn{Q_a}, \eqn{Q_b}, \eqn{Q_c}: mean squares of the errors),
#' with degrees of freedom given by Satterthwaite's formula:
#' \itemize{
#'   \item factor 1 inside factor 2: \eqn{(Q_a + (b-1) Q_b) / b};
#'   \item factor 1 inside factor 3: \eqn{(Q_a + (c-1) Q_c) / c};
#'   \item factor 1 inside factors 2 and 3:
#'     \eqn{(Q_a + (b-1) Q_b + b(c-1) Q_c) / (bc)};
#'   \item factor 2 inside factor 3 (or factors 1 and 3):
#'     \eqn{(Q_b + (c-1) Q_c) / c};
#'   \item factor 2 inside factor 1: error b; factor 3 inside any factor:
#'     error c.
#' }
#'
#' @param factor1,factor2,factor3 Vectors with the levels of factors 1, 2
#'   and 3. In the split-split-plot scheme they are, respectively, the
#'   factors of the plots, split-plots and split-split-plots.
#' @param block Vector with the blocks (RBD functions).
#' @param repet Vector with the repetitions (\code{anova_ssp_crd}). It
#'   identifies the plots: the plots of the same level of factor 1 must have
#'   different repetition numbers.
#' @param resp Numeric vector with the response variable.
#' @param quali Logical vector of length 3: \code{TRUE} for a qualitative
#'   factor, \code{FALSE} for a quantitative factor.
#' @param fac.names Names of the three factors, used in the output.
#' @param sigT Significance level of the multiple comparison tests. By
#'   default, the same as \code{sigF}.
#' @inheritParams anova_single
#' @return Invisibly, a list with \code{Anova}, \code{Shapiro.test},
#'   \code{Shapiro.result}, \code{CV}, \code{Unfolding} (list with the ANOVA
#'   of each unfolding, when an interaction is significant) and the named
#'   lists \code{Mcomp} and/or \code{Reg}.
#' @references Banzatto, D. A.; Kronka, S. N. Experimentacao Agricola.
#'   4. ed. Jaboticabal: Funep, 2006.
#'
#'   Gomes, F. P. Curso de Estatistica Experimental. 15. ed. Piracicaba:
#'   FEALQ, 2009.
#' @author Cicero Carlos de Souza Almeida
#' @seealso \code{\link{anova_fat2_crd}}, \code{\link{anova_sp2_crd}},
#'   \code{\link{des.fat3.crd}}, \code{\link{des.ssp.rbd}}
#' @examples
#' f3 <- read.csv(system.file("extdata", "fat3.csv", package = "AgriR"))
#' anova_fat3_rbd(f3$cultivar, f3$adubo, f3$espac, f3$bloco, f3$resp,
#'                fac.names = c("Cultivar", "Fertilizer", "Spacing"),
#'                graph = FALSE)
#' anova_fat3_crd(f3$cultivar, f3$adubo, f3$espac, f3$resp, graph = FALSE)
#'
#' ssp <- read.csv(system.file("extdata", "ssp.csv", package = "AgriR"))
#' anova_ssp_rbd(ssp$irrigacao, ssp$cultivar, ssp$epoca, ssp$bloco, ssp$resp,
#'               fac.names = c("Irrigation", "Cultivar", "Harvest"))
#' @name anova_three
NULL

# Pooled mean square and Satterthwaite's degrees of freedom.
.pooled <- function(ms, df, w) {
  MS <- sum(w * ms)
  list(MS = MS, df = MS^2 / sum((w * ms)^2 / df))
}

.three <- function(factor1, factor2, factor3, resp, R, rbd, design, quali,
                   fac.names, graph, graph.type, graph.col, xlab, ylab,
                   graph.den, mcomp, best.fit, sigF, sigT) {
  quali <- rep_len(as.logical(quali), 3)
  fac.names <- rep_len(as.character(fac.names), 3)
  if (any(quali)) mcomp <- .match_mcomp(mcomp)
  if (graph) graph.type <- .match_graph_type(graph.type)
  ssp <- design == "ssp"

  cat(.line(), "\nLegend:\n", sep = "")
  pos <- if (ssp) c(" (plot)", " (split-plot)", " (split-split-plot)") else
    rep("", 3)
  for (i in 1:3) cat("FACTOR ", i, pos[i], ": ", fac.names[i], "\n", sep = "")
  cat(.line(), "\n", sep = "")

  Fator1 <- factor(factor1)
  Fator2 <- factor(factor2)
  Fator3 <- factor(factor3)
  if (!is.null(R)) R <- factor(R)
  if (!ssp) {
    model <- if (rbd) stats::aov(resp ~ R + Fator1 * Fator2 * Fator3) else
      stats::aov(resp ~ Fator1 * Fator2 * Fator3)
  } else if (rbd) {
    model <- stats::aov(resp ~ R + Fator1 * Fator2 * Fator3 + R:Fator1 +
                          R:Fator1:Fator2)
  } else {
    model <- stats::aov(resp ~ Fator1 * Fator2 * Fator3 + Fator1:R +
                          Fator1:Fator2:R)
  }
  s <- .aov_terms(model)
  Fn <- c("Fator1", "Fator2", "Fator3")
  term <- function(idx) .find_row(s, Fn[idx])
  lab <- function(idx) paste(fac.names[idx], collapse = "*")

  # Errors
  MSE <- s["Residuals", "MS"]
  dfE <- s["Residuals", "DF"]
  if (ssp) {
    r.ea <- .find_row(s, c("Fator1", "R"))
    r.eb <- .find_row(s, c("Fator1", "Fator2", "R"))
    Ea <- list(MS = s[r.ea, "MS"], df = s[r.ea, "DF"])
    Eb <- list(MS = s[r.eb, "MS"], df = s[r.eb, "DF"])
    Ec <- list(MS = MSE, df = dfE)
    retest <- function(row, E) {
      s[row, "Fc"] <<- s[row, "MS"] / E$MS
      s[row, "Pr>Fc"] <<- stats::pf(s[row, "Fc"], s[row, "DF"], E$df,
                                    lower.tail = FALSE)
    }
    retest("Fator1", Ea)
    if (rbd) retest("R", Ea)
    retest("Fator2", Eb)
    retest(term(1:2), Eb)
    s[c(r.ea, r.eb), c("Fc", "Pr>Fc")] <- NA
    rows <- c(if (rbd) "R", "Fator1", r.ea, "Fator2", term(1:2), r.eb,
              "Fator3", term(c(1, 3)), term(2:3), term(1:3), "Residuals")
    labels <- c(if (rbd) "Block", fac.names[1], "Error a", fac.names[2],
                lab(1:2), "Error b", fac.names[3], lab(c(1, 3)), lab(2:3),
                lab(1:3), "Error c")
  } else {
    rows <- c(if (rbd) "R", "Fator1", "Fator2", "Fator3", term(1:2),
              term(c(1, 3)), term(2:3), term(1:3), "Residuals")
    labels <- c(if (rbd) "Block", fac.names, lab(1:2), lab(c(1, 3)),
                lab(2:3), lab(1:3), "Residuals")
  }
  tab <- .anova_out(s, rows, labels)
  .print_anova(tab)
  if (ssp) {
    cv <- list(Plot = .cv_table(Ea$MS, resp, "Coefficient of Variance (Plot)"),
               SplitPlot = .cv_table(Eb$MS, resp,
                                     "Coefficient of Variance (SplitPlot)"),
               SplitSplitPlot = .cv_table(MSE, resp,
                                          "Coefficient of Variance (SplitSplitPlot)"))
    for (k in cv) .print_cv(k)
  } else {
    cv <- .cv_table(MSE, resp)
    .print_cv(cv)
  }
  sh <- .shapiro(stats::residuals(model))
  .print_shapiro(sh)

  nl <- c(nlevels(Fator1), nlevels(Fator2), nlevels(Fator3))
  # Error used to analyse factor x inside the factors in S.
  err_for <- function(x, S) {
    if (!ssp) return(list(MS = MSE, df = dfE, name = "Residuals"))
    b <- nl[2]
    cc <- nl[3]
    E <- if (x == 1) {
      if (length(S) == 0) {
        Ea
      } else if (setequal(S, 2)) {
        .pooled(c(Ea$MS, Eb$MS), c(Ea$df, Eb$df), c(1, b - 1) / b)
      } else if (setequal(S, 3)) {
        .pooled(c(Ea$MS, Ec$MS), c(Ea$df, Ec$df), c(1, cc - 1) / cc)
      } else {
        .pooled(c(Ea$MS, Eb$MS, Ec$MS), c(Ea$df, Eb$df, Ec$df),
                c(1, b - 1, b * (cc - 1)) / (b * cc))
      }
    } else if (x == 2) {
      if (3 %in% S) {
        .pooled(c(Eb$MS, Ec$MS), c(Eb$df, Ec$df), c(1, cc - 1) / cc)
      } else {
        Eb
      }
    } else {
      Ec
    }
    nm <- "Pooled error"
    if (identical(E, Ea)) nm <- "Error a"
    if (identical(E, Eb)) nm <- "Error b"
    if (identical(E, Ec)) nm <- "Error c"
    c(E, name = nm)
  }

  Fs <- list(Fator1, Fator2, Fator3)
  fatores <- list(factor1, factor2, factor3)
  ok <- !is.na(resp)
  Mcomp <- list()
  Reg <- list()
  Unfolding <- list()
  store <- function(res, i, key) {
    if (quali[i]) Mcomp[[key]] <<- res else Reg[[key]] <<- res
  }

  # Analyses factor x inside each combination of the levels of factors S.
  unfold <- function(x, S) {
    cmb <- expand.grid(lapply(Fs[S], levels), stringsAsFactors = FALSE)
    masks <- lapply(seq_len(nrow(cmb)), function(j) {
      m <- rep(TRUE, length(resp))
      for (k in seq_along(S)) m <- m & Fs[[S[k]]] == cmb[j, k]
      m
    })
    keys <- vapply(seq_len(nrow(cmb)), function(j) {
      paste(paste0(fac.names[S], " = ", unlist(cmb[j, ])), collapse = ", ")
    }, character(1))
    ss.in <- vapply(masks, function(m) .ss_oneway(resp[m], Fs[[x]][m]),
                    numeric(1))
    df.in <- vapply(masks, function(m) length(unique(Fs[[x]][m & ok])) - 1,
                    numeric(1))
    E <- err_for(x, S)
    Fc <- (ss.in / df.in) / E$MS
    P <- stats::pf(Fc, df.in, E$df, lower.tail = FALSE)
    ut <- data.frame(DF = round(c(df.in, E$df), 2),
                     SS = round(c(ss.in, E$MS * E$df), 5),
                     MS = round(c(ss.in / df.in, E$MS), 5),
                     Fc = round(c(Fc, NA), 4), "Pr>Fc" = round(c(P, NA), 4),
                     check.names = FALSE)
    rownames(ut) <- c(paste0(fac.names[x], " : ", keys), E$name)
    title <- paste(fac.names[x], "inside of", paste(fac.names[S], collapse = " x "))
    .print_anova(ut, paste("Unfolding:", title))
    Unfolding[[title]] <<- ut
    op <- .set_layout(length(masks), graph, graph.type)
    if (!is.null(op)) on.exit(graphics::par(op))
    for (j in seq_along(masks)) {
      m <- masks[[j]]
      titulo <- paste(fac.names[x], "inside of", keys[j])
      cat("\n", titulo, "\n", sep = "")
      res <- .analyze_factor(resp[m], fatores[[x]][m], quali[x], E$df,
                             E$MS * E$df, df.in[j], ss.in[j], mcomp, sigT,
                             graph, graph.type, graph.col, graph.den, xlab,
                             ylab, titulo, best.fit)
      store(res, x, titulo)
    }
  }

  main_effects <- function(xs) {
    op <- .set_layout(length(xs), graph, graph.type)
    if (!is.null(op)) on.exit(graphics::par(op))
    for (x in xs) {
      cat("\n", fac.names[x], "\n", sep = "")
      .f_note(s[Fn[x], "Pr>Fc"], sigF, paste("the means of", fac.names[x]))
      E <- err_for(x, integer(0))
      res <- .analyze_factor(resp, fatores[[x]], quali[x], E$df, E$MS * E$df,
                             s[Fn[x], "DF"], s[Fn[x], "SS"], mcomp, sigT,
                             graph, graph.type, graph.col, graph.den, xlab,
                             ylab, fac.names[x], best.fit)
      store(res, x, fac.names[x])
    }
  }

  sig <- function(idx) {
    p <- s[term(idx), "Pr>Fc"]
    !is.na(p) && p <= sigF
  }

  if (sig(1:3)) {
    cat("\nSignificant triple interaction: analyzing the interaction\n",
        .line(), "\n", sep = "")
    for (x in 1:3) unfold(x, setdiff(1:3, x))
  } else {
    pairs <- list(c(1, 2), c(1, 3), c(2, 3))
    involved <- integer(0)
    for (pr in pairs) {
      if (sig(pr)) {
        cat("\nSignificant interaction ", lab(pr), ": analyzing the ",
            "interaction\n", .line(), "\n", sep = "")
        unfold(pr[1], pr[2])
        unfold(pr[2], pr[1])
        involved <- union(involved, pr)
      }
    }
    rest <- setdiff(1:3, involved)
    if (length(rest)) {
      cat("\nAnalyzing the main effects of the factors not involved in ",
          "significant interactions\n", .line(), "\n", sep = "")
      main_effects(rest)
    }
  }

  out <- list(Anova = tab, Shapiro.test = sh$p, Shapiro.result = sh$msg,
              CV = cv)
  if (length(Unfolding)) out$Unfolding <- Unfolding
  if (length(Mcomp)) out$Mcomp <- Mcomp
  if (length(Reg)) out$Reg <- Reg
  invisible(out)
}

#' @rdname anova_three
#' @export
anova_fat3_crd <- function(factor1, factor2, factor3, resp,
                           quali = c(TRUE, TRUE, TRUE),
                           fac.names = c("F1", "F2", "F3"), graph = TRUE,
                           graph.type = "barplotb", graph.col = "skyblue",
                           xlab = "Treatments", ylab = "Variable",
                           graph.den = 2, mcomp = "tukey", best.fit = TRUE,
                           sigF = 0.05, sigT = sigF) {
  .check_resp(resp, factor1, factor2, factor3)
  .three(factor1, factor2, factor3, resp, NULL, FALSE, "fat3", quali,
         fac.names, graph, graph.type, graph.col, xlab, ylab, graph.den,
         mcomp, best.fit, sigF, sigT)
}

#' @rdname anova_three
#' @export
anova_fat3_rbd <- function(factor1, factor2, factor3, block, resp,
                           quali = c(TRUE, TRUE, TRUE),
                           fac.names = c("F1", "F2", "F3"), graph = TRUE,
                           graph.type = "barplotb", graph.col = "skyblue",
                           xlab = "Treatments", ylab = "Variable",
                           graph.den = 2, mcomp = "tukey", best.fit = TRUE,
                           sigF = 0.05, sigT = sigF) {
  .check_resp(resp, factor1, factor2, factor3, block)
  .three(factor1, factor2, factor3, resp, block, TRUE, "fat3", quali,
         fac.names, graph, graph.type, graph.col, xlab, ylab, graph.den,
         mcomp, best.fit, sigF, sigT)
}

#' @rdname anova_three
#' @export
anova_ssp_crd <- function(factor1, factor2, factor3, repet, resp,
                          quali = c(TRUE, TRUE, TRUE),
                          fac.names = c("F1", "F2", "F3"), graph = TRUE,
                          graph.type = "barplotb", graph.col = "skyblue",
                          xlab = "Treatments", ylab = "Variable",
                          graph.den = 2, mcomp = "tukey", best.fit = TRUE,
                          sigF = 0.05, sigT = sigF) {
  .check_resp(resp, factor1, factor2, factor3, repet)
  .three(factor1, factor2, factor3, resp, repet, FALSE, "ssp", quali,
         fac.names, graph, graph.type, graph.col, xlab, ylab, graph.den,
         mcomp, best.fit, sigF, sigT)
}

#' @rdname anova_three
#' @export
anova_ssp_rbd <- function(factor1, factor2, factor3, block, resp,
                          quali = c(TRUE, TRUE, TRUE),
                          fac.names = c("F1", "F2", "F3"), graph = TRUE,
                          graph.type = "barplotb", graph.col = "skyblue",
                          xlab = "Treatments", ylab = "Variable",
                          graph.den = 2, mcomp = "tukey", best.fit = TRUE,
                          sigF = 0.05, sigT = sigF) {
  .check_resp(resp, factor1, factor2, factor3, block)
  .three(factor1, factor2, factor3, resp, block, TRUE, "ssp", quali,
         fac.names, graph, graph.type, graph.col, xlab, ylab, graph.den,
         mcomp, best.fit, sigF, sigT)
}
