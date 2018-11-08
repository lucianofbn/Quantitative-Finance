library(shiny)
library(shinydashboard)
library(plotly)
library(datasets)
library(TFX)

dashboardPage(
    dashboardHeader(title = "longshort.io", titleWidth = 230,
                    #tags$li(class="dropdown",tags$a(href="link", icon("youtube"), "My Channel", target="_blank")),
                    tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/luciano-ferreira-8bb675163/" ,icon("linkedin"), "My Profile", target="_blank")),
                    tags$li(class="dropdown",tags$a(href="https://github.com/lucianofbn", icon("github"), "Source Code", target="_blank"))
    ),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem(text = "About", tabName = "about", icon=icon("clipboard")),
        menuItem("Price now", tabName = "price", icon=icon("dollar"), badgeLabel = "new", badgeColor = "green"),
        menuItem("Charts", tabName = "charts", icon=icon("bar-chart-o"), badgeColor = "green")
        # https://fontawesome.com/icons?d=gallery
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "about",h2("How Strategy Works..."), p( 
       "Aqui vou falar sobre o projeto. Aqui vou falar sobre o projeto.Aqui vou falar sobre o projeto.Aqui vou falar sobre o projeto.Aqui vou falar sobre o projeto.Aqui vou falar sobre o projeto.Aqui vou falar sobre o projeto.
        Aqui vou falar sobre o projeto.Aqui vou falar sobre o projeto.Aqui vou falar sobre o projeto.
        Aqui vou falar sobre o projeto.Aqui vou falar sobre o projeto.Aqui vou falar sobre o projeto.Aqui vou falar sobre o projeto.
        Aqui vou falar sobre o projeto.Aqui vou falar sobre o projeto.Aqui vou falar sobre o projeto.
        Aqui vou falar sobre o projeto.Aqui vou falar sobre o projeto.Aqui vou falar sobre o projeto.Aqui vou falar sobre o projeto.
        Aqui vou falar sobre o projeto.Aqui vou falar sobre o projeto.Aqui vou falar sobre o projeto.
        Aqui vou falar sobre o projeto.Aqui vou falar sobre o projeto.Aqui vou falar sobre o projeto.
                                 
        ")),
        
        tabItem(tabName = "price", dataTableOutput("pricenow")),
        
        tabItem("charts",
                 fluidPage(
                  box(
                  title = "Enter Stock Code",width = 4, 
                  solidHeader = TRUE, 
                  status = "warning",
                  textInput("stockcodey", "Stock y:", value = "GBPUSD=X"),
                  textInput("stockcodex", "Stock x:", value = "EURUSD=X"),
                  dateRangeInput('dateRange',label = 'Date range:', format= "dd/mm/yyyy", start = Sys.Date() - 15, end = Sys.Date()),
                  selectInput("chart", "Select chart:",
                              choices = c(
                                "Residuals" = 1,
                                "Stock prices" = 2,
                                "Price difference" = 3,
                                "Linear regression y" = 4,
                                "Linear regression x" = 5
                              ),
                              selected = 1
                  ),
                  #radioButtons("typeregression", "Regression type:", c(Simple = "simple")),
                  actionButton(inputId = "btnpredict", label = "Predict")
                ),
                
                box(
                  id = "info",
                  solidHeader = TRUE,
                  title = "Info",
                  width = 8,
                  status = "success",
                  height = 180
                ),
                
                  box(
                    id = "plot",
                    solidHeader = TRUE,
                    title = "Chart",
                    width = 8,
                    status = "primary",
                    plotOutput("plot", height = 500),
                    height = 600
                  ))

      )
    )
  )
)