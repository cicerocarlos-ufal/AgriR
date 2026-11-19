# Internal helpers shared by the analysis functions (not exported).

.mcomp_methods <- c("tukey", "duncan", "lsd", "snk", "scottknott", "scheffe")
.graph_types <- c("barplotb", "barplotg", "pointplotg", "boxplotb")

.line <- function() {
  "------------------------------------------------------------------------"
}

# Normalizes the name of a multiple comparison test (accepts old aliases).
.match_mcomp <- function(mcomp) {
  m <- tolower(trimws(as.character(mcomp)[1]))
  aliases <- c(sk = "scottknott", skottknott = "scottknott",
               "scott-knott" = "scottknott", scott = "scottknott",
               hsd = "tukey")
  if (m %in% names(aliases)) m <- aliases[[m]]
  if (!m %in% .mcomp_methods) {
    stop("'mcomp' must be one of: ", paste(.mcomp_methods, collapse = ", "),
         call. = FALSE)
  }
  m
}

.match_graph_type <- function(graph.type) {
  g <- as.character(graph.type)[1]
  if (!g %in% .graph_types) {
    stop("'graph.type' must be one of: ", paste(.graph_types, collapse = ", "),
         call. = FALSE)
  }
  g
}

.match_fac_fix <- function(fac.fix) {
  f <- as.character(fac.fix)[1]
  if (!f %in% c("fac.A", "fac.B")) {
    stop("'fac.fix' must be \"fac.A\" (factor 1 inside the levels of factor 2)",
         " or \"fac.B\" (factor 2 inside the levels of factor 1)", call. = FALSE)
  }
  f
}

.check_resp <- function(resp, ...) {
  if (!is.numeric(resp)) stop("the response variable must be numeric", call. = FALSE)
  others <- list(...)
  for (o in others) {
    if (length(o) != length(resp)) {
      stop("all factors must have the same length as the response variable",
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

# Converts a quantitative factor (numeric, character or factor) to numeric.
.as_quanti <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  out <- suppressWarnings(as.numeric(x))
  if (any(is.na(out) & !is.na(x))) {
    stop("a quantitative factor (quali = FALSE) must have numeric levels",
         call. = FALSE)
  }
  out
}

# ANOVA table (one stratum) with clean row names.
.aov_terms <- function(model) {
  s <- summary(model)[[1]]
  get_col <- function(nm) {
    if (nm %in% colnames(s)) as.numeric(s[[nm]]) else rep(NA_real_, nrow(s))
  }
  tab <- data.frame(DF = get_col("Df"), SS = get_col("Sum Sq"),
                    MS = get_col("Mean Sq"), Fc = get_col("F value"),
                    P = get_col("Pr(>F)"))
  names(tab)[5] <- "Pr>Fc"
  rownames(tab) <- trimws(rownames(s))
  tab
}

# Finds the row of an ANOVA table for a term, independent of the order of
# the variables in an interaction (e.g. "Fator1:R" or "R:Fator1").
.find_row <- function(s, vars) {
  rn <- rownames(s)
  hit <- vapply(strsplit(rn, ":", fixed = TRUE),
                function(p) setequal(p, vars), logical(1))
  if (!any(hit)) {
    stop("term '", paste(vars, collapse = ":"), "' not found in the ANOVA. ",
         "Check the factors (a factor with a single level, or a design that ",
         "is not balanced, may cause this).", call. = FALSE)
  }
  rn[hit][1]
}

# Selects rows of an ANOVA table, relabels them and adds the total.
.anova_out <- function(s, rows, labels) {
  tab <- s[rows, , drop = FALSE]
  tot <- data.frame(DF = sum(s$DF), SS = sum(s$SS), MS = NA_real_,
                    Fc = NA_real_, P = NA_real_)
  names(tot)[5] <- "Pr>Fc"
  tab <- rbind(tab, tot)
  rownames(tab) <- c(labels, "Total")
  tab
}

.cv_table <- function(MS, resp, label = "Coefficient of Variance") {
  cv <- round(sqrt(MS) / mean(resp, na.rm = TRUE) * 100, 2)
  cv <- data.frame(cv, "%")
  colnames(cv) <- c("", "")
  rownames(cv) <- label
  cv
}

.shapiro <- function(res, alpha = 0.05) {
  res <- res[!is.na(res)]
  p <- tryCatch(stats::shapiro.test(res)$p.value, error = function(e) NA_real_)
  msg <- if (is.na(p)) {
    "Shapiro-Wilk test could not be computed for these residuals"
  } else if (p < alpha) {
    "WARNING: at 5% of significance, residuals can not be considered normal"
  } else {
    "According to Shapiro-Wilk normality test at 5% of significance, residuals can be considered normal"
  }
  list(p = p, msg = msg)
}

.print_anova <- function(tab, title = "Variance Analysis") {
  cat("\n", .line(), "\n", title, "\n", .line(), "\n", sep = "")
  print(tab)
}

.print_cv <- function(cv) {
  cat("\n", .line(), "\n", sep = "")
  print(cv)
}

.print_shapiro <- function(sh) {
  cat("\n", .line(), "\nShapiro-Wilk normality test\n", sep = "")
  cat("p-value: ", format(sh$p, digits = 4), "\n", sh$msg, "\n", .line(), "\n",
      sep = "")
}

.f_note <- function(p, sigF, what = "the treatment means") {
  if (!is.na(p) && p > sigF) {
    cat("\nNOTE: according to the F test (p = ", format(p, digits = 4), "), ",
        what, " can not be considered distinct at ", sigF,
        " of significance.\nInterpret the tests below with caution.\n", sep = "")
  }
  invisible(NULL)
}

# Sum of squares of a one-way classification (used to unfold interactions).
.ss_oneway <- function(y, g) {
  ok <- !is.na(y)
  y <- y[ok]
  g <- factor(g[ok])
  tot <- tapply(y, g, sum)
  n <- tapply(y, g, length)
  sum(tot^2 / n) - sum(y)^2 / length(y)
}

# Sets a multi-panel layout for base graphics; returns old par or NULL.
.set_layout <- function(n, graph, graph.type) {
  if (!isTRUE(graph) || n < 2 || graph.type %in% c("barplotg", "pointplotg")) {
    return(NULL)
  }
  nc <- if (n <= 3) 1 else 2
  nr <- ceiling(n / nc)
  graphics::par(mfrow = c(nr, nc))
}

# Runs one multiple comparison test.
.run_mcomp <- function(mcomp, y, trt, DFerror, SSerror, alpha, graph,
                       graph.type, graph.col, graph.den, xlab, ylab, main) {
  fun <- switch(mcomp, tukey = tukey, duncan = duncan, lsd = lsd, snk = snk,
                scottknott = scottknott, scheffe = scheffe)
  fun(y, trt, DFerror, SSerror, alpha = alpha, graph = graph,
      graph.col = graph.col, graph.den = graph.den, graph.type = graph.type,
      xlab = xlab, ylab = ylab, main = main)
}

# Multiple comparison (qualitative) or polynomial regression (quantitative).
.analyze_factor <- function(resp, x, quali, DFerror, SSerror, DFtreat, SStreat,
                            mcomp, sigF, graph, graph.type, graph.col,
                            graph.den, xlab, ylab, main, best.fit) {
  if (quali) {
    .run_mcomp(mcomp, resp, x, DFerror, SSerror, alpha = sigF, graph = graph,
               graph.type = graph.type, graph.col = graph.col,
               graph.den = graph.den, xlab = xlab, ylab = ylab, main = main)
  } else {
    reg.poly(resp, x, DFerror, SSerror, DFtreat, SStreat, main = main,
             xlab = xlab, ylab = ylab, graph = graph, best.fit = best.fit,
             alpha = sigF)
  }
}

# Data needed by every multiple comparison test.
.prep_mcomp <- function(y, trt) {
  if (length(y) != length(trt)) {
    stop("'y' and 'trt' must have the same length", call. = FALSE)
  }
  ok <- !is.na(y) & !is.na(trt)
  y <- as.numeric(y[ok])
  trt <- factor(trt[ok])
  if (nlevels(trt) < 2) stop("at least two treatments are required", call. = FALSE)
  means <- tapply(y, trt, mean)
  n <- tapply(y, trt, length)
  sds <- tapply(y, trt, stats::sd)
  list(y = y, trt = trt, lev = levels(trt), means = as.numeric(means),
       n = as.numeric(n), sds = as.numeric(sds))
}

# Table used by the graphs and returned by the multiple comparison tests.
.table_graph <- function(d, groups, colname) {
  tg <- data.frame(Medias = d$means, sd = d$sds)
  tg$se.sup <- tg$Medias + tg$sd
  tg$se.inf <- tg$Medias - tg$sd
  tg[[colname]] <- unname(groups[d$lev])
  rownames(tg) <- d$lev
  tg
}

.plot_mcomp <- function(tg, d, graph, graph.type, graph.col, graph.den,
                        xlab, ylab, main) {
  if (!isTRUE(graph)) return(invisible(NULL))
  graph.type <- .match_graph_type(graph.type)
  switch(graph.type,
         boxplotb = boxplotb(d$y, d$trt, tg, xlab = xlab, ylab = ylab,
                             graph.col = graph.col, main = main),
         barplotb = barplotb(tg, xlab = xlab, ylab = ylab,
                             graph.col = graph.col, graph.den = graph.den,
                             main = main),
         barplotg = barplotg(tg, xlab = xlab, ylab = ylab,
                             graph.col = graph.col, main = main),
         pointplotg = pointplotg(tg, xlab = xlab, ylab = ylab,
                                 graph.col = graph.col, main = main))
  invisible(NULL)
}

# Labels for groups: a, b, ..., z, a1, b1, ...
.group_labels <- function(n) {
  if (n <= 26) return(letters[seq_len(n)])
  k <- ceiling(n / 26)
  lab <- c(letters, as.vector(outer(letters, seq_len(k - 1), paste0)))
  lab[seq_len(n)]
}

.print_groups <- function(trt, means, groups) {
  out <- data.frame(Groups = groups, Treatments = trt, Means = means)
  print(out, row.names = FALSE)
  invisible(out)
}

# Vertical offset used to write the letters above bars/points.
.label_offset <- function(sds, top) {
  off <- mean(sds, na.rm = TRUE) / 1.5
  if (!is.finite(off) || off <= 0) {
    off <- 0.05 * max(abs(top), na.rm = TRUE)
  }
  if (!is.finite(off) || off <= 0) off <- 1
  off
}
