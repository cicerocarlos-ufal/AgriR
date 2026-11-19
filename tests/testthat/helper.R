# Runs an expression without printing to the console and without graphics.
quiet <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  out <- NULL
  utils::capture.output(out <- expr)
  out
}
ext <- function(f) read.csv(system.file("extdata", f, package = "AgriR"))
