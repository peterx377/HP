library(shiny)
library(ggplot2)
library(scales)
setwd("~/PP/covid/3.0")

# The Shiny App that displays the simulation results of daily case of covid-19

# read the true daily case count of covid
trueCase <- read.csv("./data/I/state_covid_confirmed-7ma.csv")
latestDate <- colnames(trueCase)
print(trueCase)
latestDate <- latestDate[length(latestDate)]
latestDate <- as.Date(latestDate, "X%Y.%m.%d")
# read the states
stateList = scan("./data/stateName.txt", what="", sep="\n")
stateList = stateList[1:2]
# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Covid-19 Pandemic Simulation with Hawkes Process"),
  # User Input  
  sidebarLayout (
    sidebarPanel(
      # Input of date range
      sliderInput(inputId = "dateRange", 
                  label = "Choose a time span",
                  min = as.Date('2020-3-23'),
                  max = latestDate, 
                  value = c(as.Date('2020-3-23'),as.Date('2020-06-01'))
      ),
      # Input of choice of states
      selectInput("state", "Choose a state:",
                  stateList
      ),
      actionButton("update", "Data Update"),
      actionButton("Force", "Data Force"),
    ),
    # Display Panel of results
    mainPanel(
      plotOutput(outputId = "simulation")
    )
  )
)

# Server side that updates display based on user input
server <- function(input, output, session) {
  
  output$simulation <- renderPlot({
    trueCase <- read.csv("../data/I/state_covid_confirmed-7ma.csv")
    # read data of state based on input choice on state
    simData <- read.csv(paste0("../data/I/state/", input$state, ".csv"))
    # The start/end of a period to be displayed based on user choice
    start <- difftime(input$dateRange[1],as.Date('2020-3-22'),'days')
    end <- difftime(input$dateRange[2],as.Date('2020-3-22'),'days')
    # Read the true covid case count in the date range
    trueCase <- as.numeric(c(trueCase[which(stateList %in% input$state),start:end]))
    # select the simulation result in the date range
    simData <- simData[start:end,]
    N <- length(start:end)
    # combine the simulated result with true data in one frame
    simData$true <- trueCase
    dateSeq <- seq(input$dateRange[1], input$dateRange[2], by="day")
    # Plot the data in the mainPanel
    ggplot(simData) +
      geom_bar(aes(x = dateSeq, weight = true, color="True"), fill="azure3", width = 0.6) +
      geom_line(aes(x = dateSeq, y = u, color="Average"), lwd = 2) + 
      geom_errorbar(aes(x = dateSeq, ymin = lower, ymax = upper, color="C.I."), alpha = 0.8) +
      scale_color_manual(name = "Covid Case", values = c("True"="azure3", "Average"="red", "C.I."="darkorange")) #+
  })
  observeEvent(input$update, {
    updateSliderInput(session, "dateRange", max = as.Date('2021-09-30'), value = c(as.Date('2020-3-22'),as.Date('2020-06-01')))
    output$simulation <- renderPlot({
      trueCase <- read.csv("../data/II/state_covid_confirmed-7ma.csv")
      # read data of state based on input choice on state
      simData <- read.csv(paste0("../data/II/state/", input$state, ".csv"))
      # The start/end of a period to be displayed based on user choice
      start <- difftime(input$dateRange[1],as.Date('2020-3-22'),'days')
      end <- difftime(input$dateRange[2],as.Date('2020-3-22'),'days')
      # Read the true covid case count in the date range
      trueCase <- as.numeric(c(trueCase[which(stateList %in% input$state),start:end]))
      # select the simulation result in the date range
      simData <- simData[start:end,]
      N <- length(start:end)
      # combine the simulated result with true data in one frame
      simData$true <- trueCase
      dateSeq <- seq(input$dateRange[1], input$dateRange[2], by="day")
      # Plot the data in the mainPanel
      ggplot(simData) +
        geom_bar(aes(x = dateSeq, weight = true, color="True"), fill="azure3", width = 0.6) +
        geom_line(aes(x = dateSeq, y = u, color="Average"), lwd = 2) + 
        geom_errorbar(aes(x = dateSeq, ymin = lower, ymax = upper, color="C.I."), alpha = 0.8) +
        scale_color_manual(name = "Covid Case", values = c("True"="azure3", "Average"="red", "C.I."="darkorange")) #+
    })
    
  })
  observeEvent(input$Force, {
    updateSliderInput(session, "dateRange", max = as.Date('2021-10-31'), value = c(as.Date('2020-3-22'),as.Date('2020-06-01')))
    output$simulation <- renderPlot({
      trueCase <- read.csv("../data/III/state_covid_confirmed-7ma.csv")
      # read data of state based on input choice on state
      simData <- read.csv(paste0("../data/III/state/", input$state, ".csv"))
      # The start/end of a period to be displayed based on user choice
      start <- difftime(input$dateRange[1],as.Date('2020-3-22'),'days')
      end <- difftime(input$dateRange[2],as.Date('2020-3-22'),'days')
      # Read the true covid case count in the date range
      trueCase <- as.numeric(c(trueCase[which(stateList %in% input$state),start:end]))
      # select the simulation result in the date range
      simData <- simData[start:end,]
      N <- length(start:end)
      # combine the simulated result with true data in one frame
      simData$true <- trueCase
      dateSeq <- seq(input$dateRange[1], input$dateRange[2], by="day")
      # Plot the data in the mainPanel
      ggplot(simData) +
        geom_bar(aes(x = dateSeq, weight = true, color="True"), fill="azure3", width = 0.6) +
        geom_line(aes(x = dateSeq, y = u, color="Average"), lwd = 2) + 
        geom_errorbar(aes(x = dateSeq, ymin = lower, ymax = upper, color="C.I."), alpha = 0.8) +
        scale_color_manual(name = "Covid Case", values = c("True"="azure3", "Average"="red", "C.I."="darkorange")) #+
    })
    
  })
  
}
shinyApp(ui=ui, server=server)