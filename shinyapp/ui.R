library(mafs)
library(shiny)
library(forecast)
library(data.table)

modelos <- available_models()
data(AirPassengers)

shinyUI(fluidPage(
  titlePanel("mafs package demonstration"),
  sidebarPanel(
    radioButtons(inputId = 'modelName', label = 'Please select a forecast model', choices = modelos),
    sliderInput(inputId = "horizonLength", label = "Please choose the length of the horizon forecast", value = 3, min = 1, max = 12)
  ),
  mainPanel(plotOutput(outputId = "fcast_plot"),
            tableOutput("fcast_table"),
            tableOutput("acc_table"))
)
)