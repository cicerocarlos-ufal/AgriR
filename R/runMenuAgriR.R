#' Executa o app Shiny menu_agriR
#'
#' Esta função abre a aplicação Shiny incluída no pacote AgriR.
#' @export
runMenuAgriR <- function() {
  appDir <- system.file("shiny", package = "AgriR")
  if (appDir == "") {
    stop("Não foi possível localizar o app Shiny no pacote AgriR.")
  }
  shiny::runApp(appDir)
}
