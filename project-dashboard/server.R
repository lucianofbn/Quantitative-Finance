# Load the required packages
library(shiny)
library(shinydashboard)
library(plotly)
library(datasets)
library(TFX)
library(quantmod)

if (!require(TFX) && !require(TrueFX)) {
  stop("TFX must be installed; run 'install.packages(\"TFX\")'.\n")
}
shinyServer(function(input, output, session){
  output$pricenow <- renderDataTable({
  QueryTrueFX()
  })
})

