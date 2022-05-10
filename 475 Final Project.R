library(shiny)
library(ggplot2)
library(dplyr)
library (magrittr)
library(tsibble)
library(feasts)
library(plotly)
library(fpp3)
tsibble(canadian_gas)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Final Project"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      radioButtons("radio", label = h3("Graph Options"),
                   choices = list("Seasonal", "Autocorrelation", "Decomposition", "Subseries", "Mean", "Naive", "Seasonal Naive", "Drift", "Holts", "Holts/Winters", "Manual ARIMA", "Auto ARIMA"))),
    
    # Show a plot of the generated distribution
    mainPanel(
      p("This app allows you to see the time series graph, with or without forecasts, including mean, naive, seasonal naive, draft, auomatic ARIMA, preset ARIMA, Holts, and Holts/Winters projections, or another graph of the user's choice, for data concerning the monthly production of gas in canda. The options on the left hand side of the screen for the second graph allow the user to see a second seasonal, autocorrelation, decomposition, or subseries graph."),
      plotOutput("plot1"),
      plotlyOutput("plot2"),
      textOutput("summary"))))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$summary <- renderText(
    if(input$radio == "Seasonal"){
      paste('Sesaonality plots allow someone to compare the data taken in the same season over time.')
    }
    else if(input$radio == "Autocorrelation"){
      paste('Autocorrelation plots allow someone to measure the linear relationship between lagged values of a time series.')
    }
    else if(input$radio == "Decomposition"){
      paste(' Decomposition plots allow someone to observe the trend, seasonality, and randomness within a time series on their own.')
    }
    else if(input$radio == "Subseries"){
      paste('Subseries plots allow someone to see season as their own time series.')
    }
  )
  
  output$plot1 <- renderPlot({
  if(input$radio == "Mean"){
    fit <- canadian_gas %>%
      model(MEAN())
    fit %>% 
      forecast(h = 60) %>% 
      autoplot(canadian_gas)
  }
  else if(input$radio == "Naive"){
    fit <- canadian_gas %>%
      model(NAIVE(Volume))
    fit %>% 
      forecast(h = 60) %>% 
      autoplot(canadian_gas)
  }
  else if(input$radio == "Seasonal Naive"){
    fit <- canadian_gas %>%
      model(SNAIVE(Volume))
    fit %>% 
      forecast(h = 60) %>% 
      autoplot(canadian_gas)
  }
  else if(input$radio == "Drift"){
    fit <- canadian_gas %>%
      model(RW(Volume~drift()))
    fit %>% 
      forecast(h = 60) %>% 
      autoplot(canadian_gas)
  }
  else if(input$radio == "Holts"){
    fit <- canadian_gas %>%
      model(
        ETS(Volume ~ error("A") + trend("A") + season("N"))
      )
    fit %>% 
      forecast(h = 60) %>% 
      autoplot(canadian_gas)
  }
  else if(input$radio == "Holts/Winters"){
      fit <- canadian_gas %>%
        model(
          additive = ETS(Volume ~ error("A") + trend("A") +
                           season("A")))
      fit %>% 
        forecast(h= 60) %>% 
        autoplot(canadian_gas)
  }
  else if(input$radio == "Auto ARIMA"){
    fit <- canadian_gas %>%
      model(ARIMA(Volume))
    fit %>% 
      forecast(h = 60)
      autoplot(canadian_gas)
  }
  else if(input$radio == "Manual ARIMA"){
    fit <- canadian_gas %>%
      model(ARIMA(Volume ~ pdq(1, 0 ,0) + PDQ(0, 1, 1)))
    fit %>% 
      forecast(h = 60)
      autoplot(canadian_gas)}
    else {autoplot(canadian_gas)}}) 
  
  
  
  output$plot2 <- renderPlotly({
    if(input$radio == "Seasonal") {
      canadian_gas %>% gg_season(Volume)+
        labs(title = "Seasonal")}
    else if (input$radio == "Autocorrelation") { 
      canadian_gas %>% gg_lag(Volume) +
        labs(title = "Autocorrelation")} 
    else if (input$radio == "Decomposition") {
      canadian_gas %>% 
        model(
          classical_decomposition(Volume, type = "additive")
        ) %>%
        components() %>%
        autoplot() +
        labs(title = "Decomposition")}
    else if(input$radio == "Subseries") {
      gg_subseries(canadian_gas)
    }
    
  })}

# Run the application 
shinyApp(ui = ui, server = server)
