# Algorithms that assign letters to groups of means.
# Adapted from the ExpDes package (Ferreira, Nogueira & Cavalcanti), which in
# turn adapted them from agricolae (Felipe de Mendiburu) - GPL.
# These are internal functions: they do not print anything.

# Last character of a string (ignoring trailing spaces).
lastC <- function(x) {
  y <- sub(" +$", "", x)
  p1 <- nchar(y)
  substr(y, p1, p1)
}

# Letters for tests with a single critical difference (Tukey, LSD).
order.group <- function(trt, means, N, MSerror, Tprob, std.err, parameter = 1) {
  trt <- unname(trt)
  means <- unname(means)
  N <- rep(1 / mean(1 / N), length(N))
  n <- length(means)
  z <- data.frame(trt, means, N, std.err = unname(std.err))
  w <- z[order(z[, 2], decreasing = TRUE), ]
  M <- rep("", n)
  k <- 1
  j <- 1
  cambio <- n
  cambio1 <- 0
  chequeo <- 0
  M[1] <- letters[k]
  while (j < n) {
    chequeo <- chequeo + 1
    if (chequeo > n) break
    for (i in j:n) {
      minimo <- Tprob * sqrt(parameter * MSerror * (1 / N[i] + 1 / N[j]))
      s <- abs(w[i, 2] - w[j, 2]) <= minimo
      if (s) {
        if (lastC(M[i]) != letters[k]) M[i] <- paste(M[i], letters[k], sep = "")
      } else {
        k <- k + 1
        cambio <- i
        cambio1 <- 0
        ja <- j
        for (jj in cambio:n) M[jj] <- paste(M[jj], " ", sep = "")
        M[cambio] <- paste(M[cambio], letters[k], sep = "")
        for (v in ja:cambio) {
          if (abs(w[v, 2] - w[cambio, 2]) > minimo) {
            j <- j + 1
            cambio1 <- 1
          } else {
            break
          }
        }
        break
      }
    }
    if (cambio1 == 0) j <- j + 1
  }
  data.frame(trt = as.character(w$trt), means = as.numeric(w$means),
             M = gsub(" ", "", M), N = as.numeric(w$N),
             std.err = as.numeric(w$std.err), stringsAsFactors = FALSE)
}

# Letters for tests whose critical difference depends on the number of means
# spanned by the comparison (SNK, Duncan). 'minimum' has n - 1 values.
order.stat.SNK <- function(treatment, means, minimum) {
  treatment <- unname(treatment)
  means <- unname(means)
  n <- length(means)
  z <- data.frame(treatment, means)
  w <- z[order(z[, 2], decreasing = TRUE), ]
  M <- rep("", n)
  k <- 1
  j <- 1
  r <- 1
  cambio <- n
  cambio1 <- 0
  chequeo <- 0
  M[1] <- letters[k]
  while (j < n) {
    chequeo <- chequeo + 1
    if (chequeo > n) break
    for (i in j:n) {
      r <- if (abs(j - i) == 0) 1 else abs(j - i)
      s <- abs(w[i, 2] - w[j, 2]) <= minimum[r]
      if (s) {
        if (lastC(M[i]) != letters[k]) M[i] <- paste(M[i], letters[k], sep = "")
      } else {
        k <- k + 1
        cambio <- i
        cambio1 <- 0
        ja <- j
        for (jj in cambio:n) M[jj] <- paste(M[jj], " ", sep = "")
        M[cambio] <- paste(M[cambio], letters[k], sep = "")
        for (v in ja:cambio) {
          r <- if (abs(v - cambio) == 0) 1 else abs(v - cambio)
          if (abs(w[v, 2] - w[cambio, 2]) > minimum[r]) {
            j <- j + 1
            cambio1 <- 1
          } else {
            break
          }
        }
        break
      }
    }
    if (cambio1 == 0) j <- j + 1
  }
  data.frame(trt = as.character(w$treatment), means = as.numeric(w$means),
             M = gsub(" ", "", M), stringsAsFactors = FALSE)
}

# Letters from a matrix of p-values (Scheffe).
orderPvalue <- function(treatment, means, alpha, pvalue) {
  treatment <- unname(treatment)
  means <- unname(means)
  n <- length(means)
  z <- data.frame(treatment, means)
  letras <- c(letters, LETTERS, 1:9,
              c(".", "+", "-", "*", "/", "#", "$", "%", "&", "^", "[", "]",
                ":", "@", ";", "_", "?", "!", "="), rep(" ", 2000))
  w <- z[order(z[, 2], decreasing = TRUE), ]
  M <- rep("", n)
  k <- 1
  j <- 1
  cambio <- n
  cambio1 <- 0
  chequeo <- 0
  M[1] <- letras[k]
  q <- as.numeric(rownames(w))
  while (j < n) {
    chequeo <- chequeo + 1
    if (chequeo > n) break
    for (i in j:n) {
      s <- pvalue[q[i], q[j]] > alpha
      if (s) {
        if (lastC(M[i]) != letras[k]) M[i] <- paste(M[i], letras[k], sep = "")
      } else {
        k <- k + 1
        cambio <- i
        cambio1 <- 0
        ja <- j
        M[cambio] <- paste(M[cambio], letras[k], sep = "")
        for (v in ja:cambio) {
          if (pvalue[q[v], q[cambio]] <= alpha) {
            j <- j + 1
            cambio1 <- 1
          } else {
            break
          }
        }
        break
      }
    }
    if (cambio1 == 0) j <- j + 1
  }
  if (k > 80) {
    warning(k, " groups were estimated; the number of available labels ",
            "(80) was exceeded.", call. = FALSE)
  }
  data.frame(trt = as.character(w$treatment), means = as.numeric(w$means),
             M = M, stringsAsFactors = FALSE)
}

#' Statistics of data grouped by factors
#'
#' Computes a statistic (mean by default) of one or more variables for each
#' combination of the levels of one or more factors.
#'
#' @param y A vector or data frame with the variables.
#' @param x A vector or data frame with the factors.
#' @param stat Name of the function (or the function itself) used to
#'   summarise each group, e.g. \code{"mean"}, \code{"sd"}, \code{"length"}.
#' @return A data frame with one row per group: the levels of the factors
#'   followed by the statistic of each variable.
#' @author Eric B. Ferreira, Denismar A. Nogueira and Portya P. Cavalcanti
#'   (adapted from Felipe de Mendiburu - GPL).
#' @examples
#' y <- c(10, 12, 11, 20, 21, 19)
#' trt <- rep(c("A", "B"), each = 3)
#' tapply.stat(y, trt, "mean")
#' @export
tapply.stat <- function(y, x, stat = "mean") {
  cx <- deparse(substitute(x))
  cy <- deparse(substitute(y))
  x <- data.frame(c1 = 1, x)
  y <- data.frame(v1 = 1, y)
  nx <- ncol(x)
  ny <- ncol(y)
  namex <- names(x)
  namey <- names(y)
  if (nx == 2) namex <- c("c1", cx)
  if (ny == 2) namey <- c("v1", cy)
  namexy <- c(namex, namey)
  for (i in 1:nx) x[, i] <- as.character(x[, i])
  z <- NULL
  for (i in 1:nx) z <- paste(z, x[, i], sep = "&")
  w <- NULL
  for (i in 1:ny) {
    m <- tapply(y[, i], z, stat)
    w <- cbind(w, as.matrix(m))
  }
  nw <- nrow(w)
  cc <- rownames(w)
  v <- matrix("", nrow = nw, ncol = nx)
  for (i in 1:nw) {
    for (j in 1:nx) v[i, j] <- strsplit(cc[i], "&")[[1]][j + 1]
  }
  rownames(w) <- NULL
  junto <- data.frame(v[, -1], w)
  junto <- junto[, -nx]
  names(junto) <- namexy[c(-1, -(nx + 1))]
  junto
}
