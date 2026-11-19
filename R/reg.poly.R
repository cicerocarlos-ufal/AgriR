#' Polynomial regression for quantitative treatments
#'
#' Fits sequential polynomial regression models (linear, quadratic and cubic)
#' for a quantitative factor, testing each effect against the error of the
#' experiment, and optionally plots the observed means and the fitted curves.
#'
#' The linear model is fitted whenever there are at least 2 levels, the
#' quadratic model with at least 3 levels and the cubic model with at least
#' 4 levels (the degree is limited by the treatment degrees of freedom).
#' The coefficient of determination is computed as the regression sum of
#' squares divided by the treatment sum of squares (\code{SStreat}).
#'
#' @param resp Numeric vector with the response variable.
#' @param treat Vector with the (numeric) levels of the quantitative factor.
#' @param DFerror,SSerror Degrees of freedom and sum of squares of the error.
#' @param DFtreat,SStreat Degrees of freedom and sum of squares of the
#'   treatments (or of the factor inside a level of another factor).
#' @param main Subtitle of the graph.
#' @param xlab,ylab Axis labels.
#' @param graph Logical. If \code{TRUE} (default), the graph is drawn.
#' @param best.fit Logical. If \code{TRUE}, only the best model is drawn:
#'   the model of highest degree whose highest-degree term is significant.
#'   If \code{FALSE} (default), all fitted models are drawn.
#' @param alpha Significance level used to choose the best model and to
#'   mark significant coefficients with \code{*} (default 0.05).
#' @return Invisibly, a list with the ANOVA of each model
#'   (\code{Anova.Linear}, \code{Anova.Quad}, \code{Anova.Cub}), their
#'   coefficients of determination (\code{R2.Linear}, \code{R2.Quad},
#'   \code{R2.Cub}), their coefficients (\code{Coef.Linear}, ...) and the
#'   name of the best model (\code{Best.model}).
#' @references Banzatto, D. A.; Kronka, S. N. Experimentacao Agricola.
#'   4. ed. Jaboticabal: Funep, 2006.
#'
#'   Gomes, F. P. Curso de Estatistica Experimental. 10. ed. Piracicaba:
#'   ESALQ/USP, 1982.
#' @author Cicero Carlos de Souza Almeida. Based on ExpDes (E. B. Ferreira,
#'   D. A. Nogueira, P. P. Cavalcanti).
#' @seealso \code{\link{anova_crd}}
#' @examples
#' dose <- rep(c(0, 50, 100, 150, 200), each = 4)
#' set.seed(1)
#' y <- 20 + 0.30 * dose - 0.001 * dose^2 + rnorm(20, sd = 2)
#' fit <- aov(y ~ factor(dose))
#' tab <- summary(fit)[[1]]
#' reg.poly(y, dose, tab[2, 1], tab[2, 2], tab[1, 1], tab[1, 2],
#'          best.fit = TRUE, xlab = "Dose", ylab = "Yield")
#' @export
reg.poly <- function(resp, treat, DFerror, SSerror, DFtreat, SStreat,
                     main = "", xlab = "", ylab = "", graph = TRUE,
                     best.fit = FALSE, alpha = 0.05) {
  x <- .as_quanti(treat)
  ok <- !is.na(resp) & !is.na(x)
  resp <- resp[ok]
  x <- x[ok]
  MSerror <- SSerror / DFerror
  lev <- sort(unique(x))
  nlev <- length(lev)
  if (nlev < 2) stop("the quantitative factor must have at least 2 levels",
                     call. = FALSE)
  obs.means <- as.numeric(tapply(resp, x, mean))
  maxdeg <- min(3, nlev - 1)

  dat <- data.frame(resp = resp, x1 = x, x2 = x^2, x3 = x^3)
  forms <- list(resp ~ x1, resp ~ x1 + x2, resp ~ x1 + x2 + x3)
  effects <- c("Linear Effect", "Quadratic Effect", "Cubic Effect")

  fit_deg <- function(deg) {
    fit <- stats::lm(forms[[deg]], data = dat)
    ss <- stats::anova(fit)[["Sum Sq"]][seq_len(deg)]
    Fc <- ss / MSerror
    p <- stats::pf(Fc, 1, DFerror, lower.tail = FALSE)
    df.lack <- DFtreat - deg
    ss.lack <- max(SStreat - sum(ss), 0)
    if (df.lack > 0) {
      ms.lack <- ss.lack / df.lack
      F.lack <- ms.lack / MSerror
      p.lack <- stats::pf(F.lack, df.lack, DFerror, lower.tail = FALSE)
    } else {
      ms.lack <- 0
      F.lack <- NA_real_
      p.lack <- NA_real_
    }
    tab <- data.frame(DF = c(rep(1, deg), df.lack, DFerror),
                      SS = round(c(ss, ss.lack, SSerror), 5),
                      MS = round(c(ss, ms.lack, MSerror), 5),
                      Fc = round(c(Fc, F.lack, NA), 4),
                      "p-value" = round(c(p, p.lack, NA), 5),
                      check.names = FALSE)
    rownames(tab) <- c(effects[seq_len(deg)], "Lack of fit", "Residuals")
    r2 <- if (df.lack <= 0) 1 else if (SStreat > 0) sum(ss) / SStreat else NA_real_
    list(anova = tab, r2 = r2, coef = stats::coef(fit), p = p)
  }
  fits <- lapply(seq_len(maxdeg), fit_deg)

  best <- "none"
  if (maxdeg >= 3 && isTRUE(fits[[3]]$p[3] < alpha)) {
    best <- "cubic"
  } else if (maxdeg >= 2 && isTRUE(fits[[2]]$p[2] < alpha)) {
    best <- "quadratic"
  } else if (isTRUE(fits[[1]]$p[1] < alpha)) {
    best <- "linear"
  }

  cat("\nAdjustment of polynomial models of regression\n", .line(), "\n",
      sep = "")
  titles <- c("Linear model", "Quadratic model", "Cubic model")
  for (deg in seq_len(maxdeg)) {
    cat("\n", titles[deg], "\n", sep = "")
    cf <- fits[[deg]]$coef
    names(cf) <- c("Intercept", "X", "X^2", "X^3")[seq_along(cf)]
    print(round(cf, 6))
    cat("R2 = ", round(fits[[deg]]$r2, 4), "\n", sep = "")
    print(fits[[deg]]$anova)
  }
  if (isTRUE(best.fit)) {
    cat("\n", .line(), "\nBest model: ", toupper(best), "\n", .line(), "\n",
        sep = "")
  }

  if (isTRUE(graph)) {
    xs <- seq(min(lev), max(lev), length.out = 100)
    curve_of <- function(deg) {
      b <- fits[[deg]]$coef
      out <- b[1]
      for (k in seq_len(deg)) out <- out + b[k + 1] * xs^k
      as.numeric(out)
    }
    star <- function(deg, k) if (isTRUE(fits[[deg]]$p[k] < alpha)) "*" else ""
    fmt0 <- function(v) sprintf("%.4f", v)
    fmt <- function(v) sprintf("%+.4f", v)
    eq_of <- function(deg) {
      b <- fits[[deg]]$coef
      r2 <- sprintf("%.3f", fits[[deg]]$r2)
      if (deg == 1) {
        bquote(italic("Y" == .(fmt0(b[1])) ~ .(fmt(b[2])) ~ .(star(1, 1)) ~
                        "X  |  R"^2 ~ "=" ~ .(r2)))
      } else if (deg == 2) {
        bquote(italic("Y" == .(fmt0(b[1])) ~ .(fmt(b[2])) ~ .(star(2, 1)) ~
                        "X" ~ .(fmt(b[3])) ~ .(star(2, 2)) ~ "X"^2 ~
                        " |  R"^2 ~ "=" ~ .(r2)))
      } else {
        bquote(italic("Y" == .(fmt0(b[1])) ~ .(fmt(b[2])) ~ .(star(3, 1)) ~
                        "X" ~ .(fmt(b[3])) ~ .(star(3, 2)) ~ "X"^2 ~
                        .(fmt(b[4])) ~ .(star(3, 3)) ~ "X"^3 ~
                        " |  R"^2 ~ "=" ~ .(r2)))
      }
    }
    degs <- if (isTRUE(best.fit)) {
      switch(best, none = integer(0), linear = 1L, quadratic = 2L, cubic = 3L)
    } else {
      seq_len(maxdeg)
    }
    curves <- lapply(degs, curve_of)
    ylim <- range(c(obs.means, unlist(curves)), na.rm = TRUE)
    graphics::plot(lev, obs.means, xlab = xlab, ylab = ylab, pch = 16,
                   sub = main, ylim = ylim)
    cols <- c("black", "red", "blue")
    for (k in seq_along(degs)) {
      deg <- degs[k]
      graphics::lines(xs, curves[[k]], col = cols[deg], lwd = 2)
      graphics::mtext(eq_of(deg), side = 3, line = k - 1, cex = 0.8,
                      col = cols[deg])
    }
    if (length(degs) == 0) {
      graphics::mtext("No significant regression model", side = 3, cex = 0.8)
    }
  }

  out <- list(Anova.Linear = fits[[1]]$anova, R2.Linear = fits[[1]]$r2,
              Coef.Linear = fits[[1]]$coef)
  if (maxdeg >= 2) {
    out$Anova.Quad <- fits[[2]]$anova
    out$R2.Quad <- fits[[2]]$r2
    out$Coef.Quad <- fits[[2]]$coef
  }
  if (maxdeg >= 3) {
    out$Anova.Cub <- fits[[3]]$anova
    out$R2.Cub <- fits[[3]]$r2
    out$Coef.Cub <- fits[[3]]$coef
  }
  out$Best.model <- best
  invisible(out)
}
