# Load the required packages
library(shiny)
library(shinydashboard)
library(datasets)
library(TFX)
library(quantmod)
library(xts)
library(tseries)
library(quantmod)
library(fUnitRoots)
library(urca)

if (!require(TFX) && !require(TrueFX)) {
  stop("TFX must be installed; run 'install.packages(\"TFX\")'.\n")
}

shinyServer(function(input, output, session){
  
  output$pricenow <- renderDataTable({
  QueryTrueFX()
  })

  predict <- eventReactive(input$btnpredict, {
  d <- mtcars
  plot(d$disp, d$hp)
  })

  output$plot <- renderPlot({
  predict()
  })
  
})

