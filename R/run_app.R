#' Interactive application
#'
#' Starts the interactive \pkg{shiny} application of the package, with
#' simulations (histogram, confidence intervals, comparison of means),
#' randomization of experimental designs, descriptive statistics and
#' analysis of variance of data read from CSV files.
#'
#' @param ... Arguments passed to \code{\link[shiny]{runApp}} (for example
#'   \code{launch.browser = TRUE} or \code{port = 8080}).
#' @return Called for its side effect (runs the application).
#' @author Cicero Carlos de Souza Almeida
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
#' @export
run_app <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("the package 'shiny' is required: install.packages(\"shiny\")",
         call. = FALSE)
  }
  app_dir <- system.file("shiny", package = "AgriR")
  if (!nzchar(app_dir)) {
    stop("could not find the application directory; try re-installing AgriR",
         call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
}
