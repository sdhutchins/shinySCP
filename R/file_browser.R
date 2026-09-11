render_remote_entries_ui <- function(entries, selected_path, namespace) {
  if (nrow(entries) == 0L) {
    return(tags$p(class = "empty-directory", "This directory is empty."))
  }

  table_rows <- lapply(seq_len(nrow(entries)), function(row_index) {
    entry <- entries[row_index, ]
    is_selected <- identical(entry$path, selected_path)
    type_icon <- if (identical(entry$type, "Directory")) {
      icon("folder", class = "folder-icon")
    } else {
      icon("file", class = "file-icon")
    }
    display_name <- if (identical(entry$type, "Directory")) {
      tags$button(
        type = "button",
        class = "remote-directory-link",
        `data-input-id` = namespace("open_remote_directory"),
        `data-path` = entry$path,
        paste0(entry$name, "/")
      )
    } else {
      entry$name
    }
    modified_time <- if (is.na(entry$modified)) {
      ""
    } else {
      format(entry$modified, "%Y-%m-%d %H:%M", tz = "UTC")
    }

    tags$tr(
      class = if (is_selected) {
        "remote-file-row selected"
      } else {
        "remote-file-row"
      },
      tags$td(
        tags$input(
          type = "radio",
          name = namespace("remote-entry"),
          value = entry$path,
          checked = if (is_selected) "checked" else NULL,
          `data-input-id` = namespace("selected_remote_path"),
          `aria-label` = paste("Select", entry$name)
        )
      ),
      tags$td(class = "type-cell", type_icon),
      tags$td(class = "name-cell", display_name),
      tags$td(format_file_size(entry$size, entry$type)),
      tags$td(modified_time)
    )
  })

  tags$table(
    class = "remote-file-table",
    tags$thead(
      tags$tr(
        tags$th(scope = "col", "Select"),
        tags$th(scope = "col", "Type"),
        tags$th(scope = "col", "Name"),
        tags$th(scope = "col", "Size"),
        tags$th(scope = "col", "Modified (UTC)")
      )
    ),
    tags$tbody(table_rows)
  )
}


file_browser_ui <- function(id) {
  namespace <- NS(id)

  column(
    width = 6,
    id = namespace("panel-column"),
    class = "browser-panel-column",
    tags$div(
      id = namespace("file-browser"),
      class = "file-browser panel",
      div(
        class = "connected-toolbar",
        div(
          tags$h2("File Browser"),
          tags$div(
            class = "connected-identity",
            tags$span(class = "connection-dot", `aria-hidden` = "true"),
            textOutput(namespace("connected_identity"), inline = TRUE)
          )
        ),
        actionButton(
          namespace("disconnect"),
          "Disconnect",
          icon = icon("sign-out-alt"),
          class = "btn-default disconnect-button"
        )
      ),
      div(
        class = "browser-pathbar",
        actionButton(
          namespace("parent_directory"),
          label = NULL,
          icon = icon("arrow-up"),
          class = "btn-default icon-button",
          `aria-label` = "Go to parent directory",
          title = "Go to parent directory"
        ),
        uiOutput(namespace("directory_breadcrumbs")),
        div(
          class = "pathbar-actions",
          actionButton(
            namespace("change_directory"),
            "Change directory",
            icon = icon("edit"),
            class = "btn-default"
          ),
          actionButton(
            namespace("refresh"),
            "Refresh",
            icon = icon("sync"),
            class = "btn-default"
          )
        )
      ),
      div(
        class = "browser-controls",
        checkboxInput(
          namespace("show_hidden_files"),
          "Show hidden files",
          value = FALSE
        ),
        div(
          class = "file-filter",
          tags$label(`for` = namespace("file_filter"), "Filter"),
          textInput(
            namespace("file_filter"),
            label = NULL,
            placeholder = "Filter files"
          )
        )
      ),
      tags$div(
        class = "listing-summary",
        textOutput(namespace("listing_summary"), inline = TRUE)
      ),
      tags$div(
        id = namespace("file-list-container"),
        class = "file-list-container",
        div(
          id = namespace("loading-indicator"),
          style = "display: none; text-align: center; padding: 20px;",
          div(class = "loading-spinner"),
          div(class = "loading-text", "Loading files...")
        ),
        uiOutput(namespace("file_list"))
      ),
      tags$div(
        class = "sr-only",
        textOutput(namespace("selected_entry"))
      )
    )
  )
}


file_browser_server <- function(
  id,
  ssh_session,
  is_connected,
  username,
  server_name,
  list_directory = list_remote_dir
) {
  stopifnot(
    is.reactive(ssh_session),
    is.reactive(is_connected),
    is.reactive(username),
    is.reactive(server_name),
    is.function(list_directory)
  )

  moduleServer(id, function(input, output, session) {
    browser_state <- reactiveValues(
      current_dir = NULL,
      remote_entries = data.frame(),
      selected_path = NULL
    )

    show_loading <- function() {
      shinyjs::show(session$ns("loading-indicator"))
      shinyjs::hide(session$ns("file_list"))
    }

    hide_loading <- function() {
      shinyjs::hide(session$ns("loading-indicator"))
      shinyjs::show(session$ns("file_list"))
    }

    list_files <- function(requested_path) {
      req(is_connected(), ssh_session())
      show_loading()
      on.exit(hide_loading(), add = TRUE)

      tryCatch(
        {
          listing <- list_directory(
            session = ssh_session(),
            path = requested_path,
            base_directory = browser_state$current_dir
          )

          # Preserve the existing view if path resolution or listing fails.
          browser_state$current_dir <- listing$directory
          browser_state$remote_entries <- listing$entries
          browser_state$selected_path <- NULL
        },
        error = function(condition) {
          showModal(modalDialog(
            title = "File Browsing Error",
            condition$message,
            footer = modalButton("OK"),
            easyClose = TRUE
          ))
        }
      )
    }

    observe({
      panel_id <- session$ns("panel-column")
      browser_id <- session$ns("file-browser")

      if (is_connected()) {
        shinyjs::removeClass(selector = paste0("#", panel_id), class = "col-sm-6")
        shinyjs::addClass(selector = paste0("#", panel_id), class = "col-sm-12")
        shinyjs::enable(session$ns("disconnect"))
        shinyjs::enable(session$ns("parent_directory"))
        shinyjs::enable(session$ns("refresh"))
        shinyjs::addClass(selector = paste0("#", browser_id), class = "visible")
      } else {
        shinyjs::removeClass(selector = paste0("#", panel_id), class = "col-sm-12")
        shinyjs::addClass(selector = paste0("#", panel_id), class = "col-sm-6")
        shinyjs::disable(session$ns("disconnect"))
        shinyjs::disable(session$ns("parent_directory"))
        shinyjs::disable(session$ns("refresh"))
        shinyjs::removeClass(selector = paste0("#", browser_id), class = "visible")
      }
    })

    observeEvent(is_connected(), {
      if (is_connected()) {
        list_files("~")
      } else {
        browser_state$current_dir <- NULL
        browser_state$remote_entries <- data.frame()
        browser_state$selected_path <- NULL
      }
    })

    visible_entries <- reactive({
      filter_remote_entries(
        entries = browser_state$remote_entries,
        show_hidden = input$show_hidden_files,
        query = input$file_filter
      )
    })

    output$connected_identity <- renderText({
      req(is_connected(), ssh_session(), username(), server_name())
      if (is_demo_session(ssh_session())) {
        return(sprintf("Demo filesystem \u00b7 Read-only \u00b7 %s", username()))
      }
      sprintf("Connected to %s as %s", server_name(), username())
    })

    output$directory_breadcrumbs <- renderUI({
      req(is_connected(), browser_state$current_dir)
      breadcrumbs <- build_remote_breadcrumbs(browser_state$current_dir)

      breadcrumb_items <- lapply(seq_len(nrow(breadcrumbs)), function(index) {
        is_current <- index == nrow(breadcrumbs)
        tagList(
          tags$button(
            type = "button",
            class = paste(
              "remote-breadcrumb-link",
              if (is_current) "current" else ""
            ),
            `data-input-id` = session$ns("breadcrumb_directory"),
            `data-path` = breadcrumbs$path[index],
            `aria-current` = if (is_current) "page" else NULL,
            breadcrumbs$label[index]
          ),
          if (index > 1L) {
            tags$span(class = "breadcrumb-separator", "/")
          }
        )
      })

      tags$nav(
        class = "directory-breadcrumbs",
        `aria-label` = "Remote directory",
        breadcrumb_items
      )
    })

    output$file_list <- renderUI({
      req(is_connected())
      render_remote_entries_ui(
        entries = visible_entries(),
        selected_path = browser_state$selected_path,
        namespace = session$ns
      )
    })

    output$listing_summary <- renderText({
      visible_count <- nrow(visible_entries())
      total_count <- nrow(browser_state$remote_entries)
      selected_count <- as.integer(
        !is.null(browser_state$selected_path) &&
          browser_state$selected_path %in% visible_entries()$path
      )

      sprintf(
        "Showing %d of %d entries \u00b7 %d selected",
        visible_count,
        total_count,
        selected_count
      )
    })

    output$selected_entry <- renderText({
      if (is.null(browser_state$selected_path)) {
        return("No file or directory selected")
      }

      paste("Selected:", browser_state$selected_path)
    })

    observeEvent(input$change_directory, {
      req(browser_state$current_dir)
      showModal(modalDialog(
        title = "Change directory",
        textInput(
          session$ns("directory_path"),
          "Remote path",
          value = browser_state$current_dir,
          width = "100%"
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            session$ns("confirm_directory"),
            "Go",
            class = "btn-primary"
          )
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$confirm_directory, {
      req(input$directory_path)
      requested_path <- input$directory_path
      removeModal()
      list_files(requested_path)
    })

    observeEvent(input$refresh, {
      req(browser_state$current_dir)
      list_files(browser_state$current_dir)
    })

    observeEvent(input$parent_directory, {
      req(browser_state$current_dir)
      parent_directory <- if (identical(browser_state$current_dir, "/")) {
        "/"
      } else {
        dirname(browser_state$current_dir)
      }
      list_files(parent_directory)
    })

    observeEvent(input$breadcrumb_directory, {
      req(browser_state$current_dir)
      valid_paths <- build_remote_breadcrumbs(browser_state$current_dir)$path
      if (input$breadcrumb_directory %in% valid_paths) {
        list_files(input$breadcrumb_directory)
      }
    })

    observeEvent(input$selected_remote_path, {
      selected_index <- match(
        input$selected_remote_path,
        browser_state$remote_entries$path
      )
      if (!is.na(selected_index)) {
        browser_state$selected_path <-
          browser_state$remote_entries$path[selected_index]
      }
    })

    observeEvent(input$open_remote_directory, {
      selected_index <- match(
        input$open_remote_directory,
        browser_state$remote_entries$path
      )
      if (
        !is.na(selected_index) &&
          identical(
            browser_state$remote_entries$type[selected_index],
            "Directory"
          )
      ) {
        list_files(browser_state$remote_entries$path[selected_index])
      }
    })

    list(
      disconnect = reactive(input$disconnect),
      selected_path = reactive(browser_state$selected_path),
      current_directory = reactive(browser_state$current_dir)
    )
  })
}
