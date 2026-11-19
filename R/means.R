#' Arithmetic, geometric, harmonic and weighted means
#'
#' Simple functions to compute different kinds of means of a numeric vector.
#'
#' @param x Numeric vector. For \code{geo_mean} and \code{har_mean} all
#'   values must be positive.
#' @param w Numeric vector of weights, with the same length as \code{x}.
#' @param na.rm Logical. If \code{TRUE} (default), missing values are removed
#'   before the computation.
#' @return A single numeric value.
#' @author Cicero Carlos de Souza Almeida
#' @examples
#' x <- c(3, 4, 4, 4, 5, 5, 6, 8, 8, 9, 9, 10, 11, 12)
#' w <- c(5, 7, 6, 5, 2, 1, 5, 4, 8, 3, 2, 3, 8, 9)
#' ari_mean(x)
#' geo_mean(x)
#' har_mean(x)
#' pon_mean(x, w)
#' @name means
NULL

.clean_numeric <- function(x, na.rm) {
  if (!is.numeric(x)) stop("'x' must be numeric", call. = FALSE)
  if (na.rm) x <- x[!is.na(x)]
  x
}

#' @rdname means
#' @export
ari_mean <- function(x, na.rm = TRUE) {
  x <- .clean_numeric(x, na.rm)
  sum(x) / length(x)
}

#' @rdname means
#' @export
geo_mean <- function(x, na.rm = TRUE) {
  x <- .clean_numeric(x, na.rm)
  if (any(x <= 0, na.rm = TRUE)) {
    stop("the geometric mean requires all values to be > 0", call. = FALSE)
  }
  exp(sum(log(x)) / length(x))
}

#' @rdname means
#' @export
har_mean <- function(x, na.rm = TRUE) {
  x <- .clean_numeric(x, na.rm)
  if (any(x <= 0, na.rm = TRUE)) {
    stop("the harmonic mean requires all values to be > 0", call. = FALSE)
  }
  length(x) / sum(1 / x)
}

#' @rdname means
#' @export
pon_mean <- function(x, w, na.rm = TRUE) {
  if (!is.numeric(x) || !is.numeric(w)) {
    stop("'x' and 'w' must be numeric", call. = FALSE)
  }
  if (length(x) != length(w)) {
    stop("'x' and 'w' must have the same length", call. = FALSE)
  }
  if (na.rm) {
    ok <- !is.na(x) & !is.na(w)
    x <- x[ok]
    w <- w[ok]
  }
  sum(x * w) / sum(w)
}
