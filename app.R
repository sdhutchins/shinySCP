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
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  id = "main-nav",
  title = "shinySCP",
  theme = shinytheme("yeti"),
  collapsible = TRUE,
  useShinyjs(),  # Enable shinyjs

  # Main Page with Split Layout
  tabPanel(
    title = "Home",
    div(
      class = "main-container",
      fluidRow(
        # Login Panel (Left Side)
        column(6,
          tags$div(
            class = "panel",
            tags$h2("Login to Server"),
            textInput("username", label = h4("Username"), value = ""),
            textInput("server", label = h4("Server"), value = ""),
            passwordInput("password", label = h4("Password"), value = ""),
            div(
              style = "margin-top: 20px;",
              actionButton("submit", "Login", class = "btn-primary"),
              actionButton("reset", "Clear", class = "btn-default"),
              actionButton("disconnect", "Disconnect", class = "btn-danger")
            ),
            tags$div(
              id = "connection-status",
              class = "connection-status",
              textOutput("session")
            )
          )
        ),
        # File Browser Panel (Right Side)
        column(6,
          tags$div(
            id = "file-browser",
            class = "file-browser panel",
            tags$h2("File Browser"),
            div(
              class = "path-container",
              div(
                class = "path-input",
                textInput("current_path", label = h4("Current Path"), value = "~")
              ),
              div(
                class = "browse-button",
                actionButton("browse", "Browse", class = "btn-primary")
              )
            ),
            tags$div(
              id = "file-list-container",
              class = "file-list-container",
              div(
                id = "loading-indicator",
                style = "display: none; text-align: center; padding: 20px;",
                div(
                  class = "loading-spinner"
                ),
                div(
                  class = "loading-text",
                  "Loading files..."
                )
              ),
              verbatimTextOutput("file_list")
            )
          )
        )
      )
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
  # Reactive values to store session and connection status
  x <- reactiveValues(
    username = NULL, 
    server = NULL, 
    password = NULL,
    ssh_session = NULL,
    is_connected = FALSE,
    current_dir = NULL
  )

  # Function to show/hide loading
  show_loading <- function() {
    shinyjs::show("loading-indicator")
    shinyjs::hide("file_list")
  }
  
  hide_loading <- function() {
    shinyjs::hide("loading-indicator")
    shinyjs::show("file_list")
  }

  # Function to update current directory
  update_current_dir <- function() {
    req(x$is_connected, x$ssh_session)
    tryCatch({
      x$current_dir <- get_remote_pwd(x$ssh_session)
      updateTextInput(session, "current_path", value = x$current_dir)
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        "Failed to get current directory",
        footer = modalButton("OK"),
        easyClose = TRUE
      ))
    })
  }

  # Function to list directory contents
  list_files <- function() {
    req(x$is_connected, x$ssh_session)
    
    # Show loading
    show_loading()
    
    tryCatch({
      # Update current directory first
      update_current_dir()
      
      # Then list files
      files <- list_remote_dir(x$ssh_session, input$current_path)
      
      # Hide loading and show results
      hide_loading()
      
      output$file_list <- renderPrint({
        if (length(files) > 0) {
          cat("Current directory:", x$current_dir, "\n")
          cat("----------------------------------------\n")
          cat(paste(files, collapse = "\n"))
        } else {
          cat("No files found or directory is empty")
        }
      })
    }, error = function(e) {
      hide_loading()
      output$file_list <- renderPrint({
        cat("Error listing directory:", e$message)
      })
      showModal(modalDialog(
        title = "File Browsing Error",
        paste("Error listing directory:", e$message),
        footer = modalButton("OK"),
        easyClose = TRUE
      ))
    })
  }

  # Enable/disable disconnect button based on connection status and initialize UI
  observe({
    if (x$is_connected) {
      shinyjs::enable("disconnect")
      shinyjs::enable("browse")
      # Show file browser and update status styling
      shinyjs::addClass(selector = "#file-browser", class = "visible")
      shinyjs::addClass(selector = "#connection-status", class = "connected")
      shinyjs::removeClass(selector = "#connection-status", class = "disconnected")
      shinyjs::removeClass(selector = "#connection-status", class = "initial")
    } else if (is.null(x$username) && is.null(x$server) && is.null(x$password)) {
      # Initial state
      shinyjs::disable("disconnect")
      shinyjs::disable("browse")
      shinyjs::removeClass(selector = "#file-browser", class = "visible")
      shinyjs::removeClass(selector = "#connection-status", class = "connected")
      shinyjs::removeClass(selector = "#connection-status", class = "disconnected")
      shinyjs::addClass(selector = "#connection-status", class = "initial")
    } else {
      # Disconnected state
      shinyjs::disable("disconnect")
      shinyjs::disable("browse")
      shinyjs::removeClass(selector = "#file-browser", class = "visible")
      shinyjs::removeClass(selector = "#connection-status", class = "connected")
      shinyjs::removeClass(selector = "#connection-status", class = "initial")
      shinyjs::addClass(selector = "#connection-status", class = "disconnected")
    }
  })

  observeEvent(input$submit, {
    x$username <- input$username
    x$server <- input$server
    x$password <- input$password
    
    # Try to establish connection
    tryCatch({
      x$ssh_session <- connect(x$username, x$server, x$password)
      x$is_connected <- TRUE
      
      # Get initial directory
      update_current_dir()
      
      # Show success modal
      showModal(modalDialog(
        title = "Connection Successful",
        paste("Successfully connected to", x$server, "as", x$username),
        footer = modalButton("OK"),
        easyClose = TRUE
      ))
      
      # List files
      list_files()
    }, error = function(e) {
      x$is_connected <- FALSE
      # Show error modal
      showModal(modalDialog(
        title = "Connection Failed",
        paste("Failed to connect:", e$message),
        footer = modalButton("OK"),
        easyClose = TRUE
      ))
    })
  })

  # Handle disconnect button
  observeEvent(input$disconnect, {
    if (!is.null(x$ssh_session)) {
      ssh_disconnect(x$ssh_session)
      x$ssh_session <- NULL
      x$is_connected <- FALSE
      # Show disconnect modal
      showModal(modalDialog(
        title = "Disconnected",
        "Successfully disconnected from server",
        footer = modalButton("OK"),
        easyClose = TRUE
      ))
      # Clear input fields
      updateTextInput(session, "username", value = "")
      updateTextInput(session, "server", value = "")
      updateTextInput(session, "password", value = "")
    }
  })

  observeEvent(input$reset, {
    updateTextInput(session, "username", value = "")
    updateTextInput(session, "server", value = "")
    updateTextInput(session, "password", value = "")
  })

  output$session <- renderText({
    if (is.null(x$username) && is.null(x$server) && is.null(x$password)) {
      "Please enter your credentials to connect"
    } else if (x$is_connected) {
      sprintf("Connected to %s as %s", x$server, x$username)
    } else {
      "Not connected"
    }
  })
  
  # Handle directory browsing
  observeEvent(input$browse, {
    list_files()
  })
  
  # Clean up session when app closes
  session$onSessionEnded(function() {
    if (!is.null(x$ssh_session)) {
      ssh_disconnect(x$ssh_session)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
