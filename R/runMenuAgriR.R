#' Run the Shiny app Menu AgriR
#' 
#' Opens the Shiny app included in the AgriR package.
#' If a browser is available, it will launch automatically.
#' Otherwise, the URL will be printed for manual access.
#' @export
runMenuAgriR <- function() {
  appDir <- system.file("shiny", package = "AgriR")
  if (appDir == "") stop("App directory not found in the package")
  
  # tenta determinar navegador
  browser_path <- getOption("browser")
  
  # função de fallback caso browser não esteja definido
  launch_browser <- TRUE
  if (is.null(browser_path) || browser_path == "") {
    message("No browser detected. You can manually open the app at the URL printed below.")
    launch_browser <- FALSE
  }
  
  # executa o app
  shiny::runApp(
    appDir,
    launch.browser = launch_browser
  )
}

