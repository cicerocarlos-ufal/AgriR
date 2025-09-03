Install

remotes::install_github("cicerocarlos-ufal/AgriR")

library(AgriR)

#adicionar o shiny

library(shiny)

runApp(system.file("shiny", package = "AgriR")
