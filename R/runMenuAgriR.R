#' Run the Shiny app Menu AgriR
#' 
#' Opens the Shiny app included in the AgriR package.
#' If a browser is available, it will launch automatically.
#' Otherwise, the URL will be printed for manual access.
#' @export
runMenuAgriR <- function() {
  appDir <- system.file("shiny", "menu_agriR", package = "AgriR")
  if (appDir == "") stop("App directory not found in the package")
  
  shiny::runApp(appDir, launch.browser = TRUE)  # força abrir no navegador
}

