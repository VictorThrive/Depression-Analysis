#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#Load libraries
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)

#Importing dataset
bike <- read.csv('bike_sharing.csv')
bike$yr <- as.factor(bike$yr)
bike$mnth <- factor(bike$mnth, levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
bike$weekday <- factor(bike$weekday, levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))
bike$season <- factor(bike$season, levels = c('Spring', 'Summer', 'Fall', 'Winter'))


# Define UI for application that draws a histogram
ui <- fluidPage(
  tabsetPanel(
    tabPanel(title ="Data",
             #Histogram filter
             box(status = 'primary', title = 'Filter for the histogram plot',
                 selectInput('num', "Numerical variables:", c('Temperature', 'Feeling temperature', 'Humidity', 'Wind speed', 'Casual', 'New', 'Total')),
                 footer = 'Histogram plot for numerical variables'),
             #Frecuency plot filter
             box(status = 'primary', title = 'Filter for the frequency plot',
                 selectInput('cat', 'Categorical variables:', c('Season', 'Year', 'Month', 'Hour', 'Holiday', 'Weekday', 'Working day', 'Weather')),
                 footer = 'Frequency plot for categorical variables'),
             #Boxes to display the plots
             box(plotOutput('histPlot')),
             box(plotOutput('freqPlot'))
             ),
    tabPanel("Plots"),
    tabPanel("Model")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application
shinyApp(ui = ui, server = server)
