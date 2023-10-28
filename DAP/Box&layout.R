#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)


#header
header <- dashboardHeader(disable = TRUE)
#sidebar
sidebar <- dashboardSidebar(
  sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "Search..."),
  sliderInput("slider", "Slider input:", 1, 100, 50),
  textInput("text", "Text input:")
)
# body
body <- dashboardBody(
  # infoBoxes with fill=FALSE
  fluidRow(
    # A static infoBox
    infoBox("New Order 1", 10 * 2, icon = icon("credit-card")),
    # Dynamic infoBoxes
    infoBoxOutput("progressBox"),
    infoBoxOutput("approvalBox"),
    # A static infoBox
    infoBox("New Orders 2", 10 * 2, icon = icon("credit-card")),
    # Dynamic infoBoxes
    infoBoxOutput("progressBox"),
    infoBoxOutput("approvalBox"),
    # A static infoBox
    infoBox("New Order 3", 10 * 2, icon = icon("credit-card")),
    # Dynamic infoBoxes
    infoBoxOutput("progressBox"),
    infoBoxOutput("approvalBox")
  ),
  # infoBoxes with fill=TRUE
  fluidRow(
    infoBox("New Order 4", 10 * 2, icon = icon("credit-card"), fill = TRUE),
    infoBoxOutput("progressBox2"),
    infoBoxOutput("approvalBox2"),
    infoBox("New Orders 5", 10 * 2, icon = icon("credit-card"), fill = TRUE),
    infoBoxOutput("progressBox2"),
    infoBoxOutput("approvalBox2"),
    infoBox("New Order 6", 10 * 2, icon = icon("credit-card"), fill = TRUE),
    infoBoxOutput("progressBox2"),
    infoBoxOutput("approvalBox2")
  ),
  fluidRow(
    # Clicking this will increment the progress amount
    box(width = 4, actionButton("count", "Increment progress 1")),
    # Clicking this will increment the progress amount
    box(width = 4, actionButton("count", "Increment progress 2")),
    # Clicking this will increment the progress amount
    box(width = 4, actionButton("count", "Increment progress 3"))
  ),
  fluidRow(
    # A static valueBox
    valueBox(10 * 2, "New Orders 7", icon = icon("credit-card")),
    # Dynamic valueBoxes
    valueBoxOutput("progressBox"),
    valueBoxOutput("approvalBox"),
    # A static valueBox
    valueBox(10 * 2, "New Orders 8", icon = icon("credit-card")),
    # Dynamic valueBoxes
    valueBoxOutput("progressBox"),
    valueBoxOutput("approvalBox"),
    # A static valueBox
    valueBox(10 * 2, "New Orders 9", icon = icon("credit-card")),
    # Dynamic valueBoxes
    valueBoxOutput("progressBox"),
    valueBoxOutput("approvalBox")
  ),
  fluidRow(
    # Clicking this will increment the progress amount
    box(width = 4, actionButton("count", "Increment progress")),
    # Clicking this will increment the progress amount
    box(width = 4, actionButton("count", "Increment progress")),
    # Clicking this will increment the progress amount
    box(width = 4, actionButton("count", "Increment progress"))
  ),
  fluidRow(
    box(
      title = "Inputs", status = "primary", solidHeader = TRUE, background = "maroon",
      "Box content here", br(), "More box content",
      textInput("text2", "Text input:")),
    box(
      title = "Inputs", status ="warning",solidHeader = TRUE,background = "navy",
      "Box content here", br(), "More box content",
      sliderInput("slider2", "Slider input:", 1, 100, 50),
      textInput("text2", "Text input:")
    )
  ),
  fluidRow(
    tabBox(
      title = "First tabBox",
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1", height = "250px",
      tabPanel("Tab1", "First tab content"),
      tabPanel("Tab2", "Tab content 2")
    ),
    tabBox(
      side = "right", height = "250px",
      selected = "Tab3",
      tabPanel("Tab1", "Tab content 1"),
      tabPanel("Tab2", "Tab content 2"),
      tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
    )
  ),
  fluidRow(
    tabBox(
      # Title can include an icon
      title = tagList(shiny::icon("gear"), "tabBox status"),
      tabPanel("Tab1",
               "Currently selected tab from first box:",
               verbatimTextOutput("tabset1Selected")
      ),
      tabPanel("Tab2", "Tab content 2")
    )
  )
)

#dashboard page
dashboardPage(header, sidebar, body)

# Define UI for application that draws a histogram
ui <- dashboardPage(header, sidebar, body)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$tabset1Selected <- renderText({
    input$tabset1
  })
  output$progressBox <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple"
    )
  })
  output$approvalBox <- renderInfoBox({
    infoBox(
      "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  # Same as above, but with fill=TRUE
  output$progressBox2 <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple", fill = TRUE
    )
  })
  output$approvalBox2 <- renderInfoBox({
    infoBox("Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
            color = "yellow", fill = TRUE
    )
  })
  output$progressBox <- renderValueBox({
    valueBox(
      paste0(25 + input$count, "%"), "Progress", icon = icon("list"),
      color = "purple"
    )
  })
  output$approvalBox <- renderValueBox({
    valueBox(
      "80%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
