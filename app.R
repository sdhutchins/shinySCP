#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(shinythemes)
library(shinyjs)
library(markdown)
source("helpers.R")

# Define UI for application that logs into a server.
ui <- navbarPage(
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Cabin|Oxygen+Mono|Roboto+Mono|Share+Tech+Mono|Source+Code+Pro"),
    tags$style("* {font-family: 'Cabin', sans-serif !important;}", type = "text/css"),
    tags$style(".fa, .far, .fas {font-family: 'Font Awesome 5 Free' !important;}", type = "text/css")
  ),
  id = "main-nav",
  title = "shinySCP",
  theme = shinytheme("yeti"),
  collapsible = TRUE,
  # footer = tags$div(tags$p('<center>Made by Shaurita. D. Hutchins</center>')),

  # Login Page
  tabPanel(
    title = "Login",
    # Sidebar with a slider input for number of bins
    fluidRow(
      column(4),
      column(4,
        align = "center",
        tags$div(
          class = "well",
          tags$h2("Login to a server"),
          tags$br(),
          textInput("username", label = h4("Username"), value = ""),
          textInput("server", label = h4("Server"), value = ""),
          textInput("password", label = h4("Password"), value = ""),
          actionButton("submit", "Login"),
          actionButton("reset", "Clear"),
          tags$br(),
          tags$br(),
          textOutput("session")
        )
      ),
      column(4)
    )
  ),
  # About Page
  tabPanel(
    id = "About",
    title = "About",
    fluidRow(
      column(3),
      column(6,
        align = "left",
        tags$div(class = "container-fluid", includeMarkdown("about.md"))
      ),
      column(3)
    )
  )
)

# Define server logic required to login to a server
server <- function(input, output, session) {
  x <- reactiveValues(username = NULL, server = NULL, password = NULL)

  observeEvent(input$submit, {
    x$username <- input$username
    x$server <- input$server
    x$password <- input$password
  })

  observeEvent(input$reset, {
    updateTextInput(session, "username", value = "")
    updateTextInput(session, "server", value = "")
    updateTextInput(session, "password", value = "")
  })

  output$session <- renderPrint({
    if (!is.null(x$username) | !is.null(x$server) | !is.null(x$password)) {
      (
        connect(x$username, x$server, x$password)
      )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
