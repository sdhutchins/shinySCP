#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(shinythemes)
source("helpers.R")

# Define UI for application that logs into a server.
ui <- navbarPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Cabin|Oxygen+Mono|Roboto+Mono|Share+Tech+Mono|Source+Code+Pro"
  ),
  tags$style("* {font-family: 'Cabin', sans-serif !important;}", type = "text/css")),
  title = "shinySCP",
  theme = shinytheme("yeti"),

  # Application title
  tabPanel(
    "SSH Login",

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(tags$style(".well {background-color: #fff;}", type = "text/css"),
        textInput("username", label = h4("Username"), value = ""),
        textInput("server", label = h4("Server"), value = ""),
        textInput("password", label = h4("Password"), value = ""),
        actionButton("submit", "Log in"),
        actionButton("reset", "Clear"),
        actionButton("logout", "Disconnect"),
        width = 3
      ),

      # Show files/directories
      mainPanel(
        # TODO: Print the session information
        textOutput("session")
      )
    )
  ), tabPanel(
    "About",
    tags$div(class = "container-fluid", includeMarkdown("assets/markdown/about.md"))
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
    if (is.null(x$username) | is.null(x$server) | is.null(x$password)) return(NULL)
    connect(x$username, x$server, x$password)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
