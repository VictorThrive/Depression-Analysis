library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  # Tab item for plots
  tabItem(
    "plots",
    box(
      helpText('FUNAAB Student Depression Assessment Questionnaire was used to collect data from undergraduate students from the ten colleges.'),
      helpText('The data contains 239 observations and 12 attributes related to students demographic info, stress score, stress level, and academic performance.'),
      status = 'primary',
      solidHeader = TRUE,
      width = 12,
      title = "FUNAAB Students Depression Assessment Survey Data",
      "The data can be filtered by the selected variables below"
    ),
    column(3,
           selectInput("cat", "Select variable",
                       choices = c("College", "Department", "Gender", "Level", "Depression"))
    ),
    column(9, plotOutput("plot"))
  )
)

# Define server
server <- function(input, output) {
  # Use reactive expression to generate the filtered table
  tab <- reactive({
    filtered_data <- df[, c("College", "Department", "Gender", "Level", "Depression")]
    filtered_data |>
      count(!!as.symbol(input$cat))
  })

  output$plot <- renderPlot({
    # Use tab() as data frame for ggplot
    ggplot(tab(), aes(x = reorder(!!as.symbol(input$cat), -n), y = n)) +
      geom_bar(stat = "identity", fill = c(rep("grey", 7), "#99020f", "grey", "#0047AB")) +
      geom_text(aes(label = n), vjust = -0.5, color = "#99020f", size = 5) +
      labs(
        subtitle = "College of Physical had 36% response",
        x = input$cat,
        y = "Number of responses"
      ) +
      theme(
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)
      )
  })
}

# Run app
shinyApp(ui, server)
