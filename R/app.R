#' shinySCP package
#'
#' @keywords internal
#' @import shiny
#' @importFrom pkgload load_all
"_PACKAGE"


shiny_scp_ui <- function(ssh_login_enabled = ssh_login_is_enabled()) {
  stopifnot(
    is.logical(ssh_login_enabled),
    length(ssh_login_enabled) == 1L,
    !is.na(ssh_login_enabled)
  )

  stylesheet_path <- system.file(
    "app/www/styles.css",
    package = "shinySCP"
  )
  about_path <- system.file("app/about.md", package = "shinySCP")

  if (!nzchar(stylesheet_path) || !nzchar(about_path)) {
    stop("The shinySCP application assets could not be located.", call. = FALSE)
  }

  ui <- navbarPage(
    header = tagList(
      tags$head(
        tags$link(
          rel = "stylesheet",
          type = "text/css",
          href = paste0(
            "https://fonts.googleapis.com/css2?",
            "family=Inter:wght@400;500;600;700&display=swap"
          )
        ),
        includeCSS(stylesheet_path),
        tags$script(HTML(
          "function setLoginLoading(isLoading) {
          var loginButton = $('#submit');
          var loginControls = $('#login-panel-column :input');
          var loginPanel = $('.login-panel');

          if (isLoading) {
            if (!loginButton.data('idle-html')) {
              loginButton.data('idle-html', loginButton.html());
            }

            loginControls.prop('disabled', true);
            loginButton
              .attr('aria-busy', 'true')
              .html(
                '<span class=\"login-button-spinner\" ' +
                'aria-hidden=\"true\"></span> Connecting...'
              );
            loginPanel.attr('aria-busy', 'true');
            $('body').addClass('is-connecting');
            return;
          }

          loginControls
            .not('[data-demo-disabled=\"true\"]')
            .prop('disabled', false);
          loginButton
            .removeAttr('aria-busy')
            .html(loginButton.data('idle-html') || 'Login');
          loginPanel.removeAttr('aria-busy');
          $('body').removeClass('is-connecting');
        }

        function setLoginValidation(message) {
          var loginFields = $('#username, #server, #password');
          var errorContainer = $('#login-errors');

          loginFields.removeAttr('aria-invalid');

          if (!message.fields || message.fields.length === 0) {
            errorContainer.prop('hidden', true).text('');
            return;
          }

          message.fields.forEach(function(fieldId) {
            $('#' + fieldId).attr('aria-invalid', 'true');
          });
          errorContainer.text(message.message).prop('hidden', false);
          $('#' + message.fields[0]).trigger('focus');
        }

        $(document).on('click', '#submit', function() {
          var hasEmptyField = $('#username, #server, #password')
            .toArray()
            .some(function(field) {
              return field.value.length === 0;
            });

          if (hasEmptyField) {
            return;
          }

          // Let Shiny record the click before disabling the action button.
          window.setTimeout(function() {
            setLoginLoading(true);
          }, 0);
        });

        $(document).on('shiny:connected', function() {
          Shiny.addCustomMessageHandler(
            'set-login-loading',
            function(message) {
              setLoginLoading(message.active);
            }
          );
          Shiny.addCustomMessageHandler(
            'set-login-validation',
            setLoginValidation
          );
        });

        $(document).on('input', '#username, #server, #password', function() {
          $(this).removeAttr('aria-invalid');

          if ($('#username, #server, #password').filter('[aria-invalid=true]').length === 0) {
            $('#login-errors').prop('hidden', true).text('');
          }
        });

        var modalTrigger = null;
        var modalTitleSequence = 0;

        $(document).on('show.bs.modal', '.modal', function() {
          modalTrigger = document.activeElement;
        });

        $(document).on('shown.bs.modal', '.modal', function() {
          var modal = $(this);
          var modalTitle = modal.find('.modal-title').first();
          var focusTarget;

          modal.attr({
            'role': 'dialog',
            'aria-modal': 'true'
          });

          if (modalTitle.length > 0) {
            if (!modalTitle.attr('id')) {
              modalTitleSequence += 1;
              modalTitle.attr('id', 'modal-title-' + modalTitleSequence);
            }
            modal.attr('aria-labelledby', modalTitle.attr('id'));
          }

          focusTarget = modal
            .find('input, select, textarea, button, [href]')
            .filter(':visible:not([disabled])')
            .first();

          if (focusTarget.length > 0) {
            focusTarget.trigger('focus');
          }
        });

        $(document).on('hidden.bs.modal', '.modal', function() {
          var fallbackTarget;

          if (
            modalTrigger &&
            document.contains(modalTrigger) &&
            $(modalTrigger).is(':visible:not([disabled])')
          ) {
            modalTrigger.focus();
          } else {
            fallbackTarget = $('.disconnect-button, #username')
              .filter(':visible:not([disabled])')
              .first();

            if (fallbackTarget.length > 0) {
              fallbackTarget.trigger('focus');
            }
          }
          modalTrigger = null;
        });

        $(document).on('change', '.remote-file-row input[type=radio]', function() {
          Shiny.setInputValue(
            $(this).data('input-id'),
            this.value,
            {priority: 'event'}
          );
        });

        $(document).on('click', '.remote-directory-link', function() {
          Shiny.setInputValue(
            $(this).data('input-id'),
            $(this).data('path'),
            {priority: 'event'}
          );
        });

        $(document).on('click', '.remote-breadcrumb-link', function() {
          Shiny.setInputValue(
            $(this).data('input-id'),
            $(this).data('path'),
            {priority: 'event'}
          );
        });

        $(document).on('click', '.remote-file-row', function(event) {
          if ($(event.target).closest('button, input').length > 0) {
            return;
          }
          $(this)
            .find('input[type=radio]')
            .prop('checked', true)
            .trigger('change');
        });"
        ))
      ),
      shinyjs::useShinyjs()
    ),
    id = "main-nav",
    title = "shinySCP",
    theme = shinythemes::shinytheme("yeti"),
    collapsible = TRUE,
    lang = "en",
    footer = tags$footer(
      class = "app-footer",
      tags$p(
        class = "footer-copy",
        paste0(
          paste(
            "shinySCP was created for demo purposes.",
            "Learn more about the developer,"
          ),
          " "
        ),
        tags$a(
          href = "https://www.shauritahutchins.com/",
          "Shaurita D. Hutchins."
        )
      )
    ),
    tabPanel(
      title = "Home",
      div(
        class = "main-container login-view",
        fluidRow(
          column(
            width = 6,
            offset = 3,
            id = "login-panel-column",
            tags$div(
              class = "panel login-panel",
              tags$h2(if (ssh_login_enabled) {
                "Login to Server"
              } else {
                "Explore the Demo"
              }),
              tags$div(
                id = "login-guidance",
                class = "login-privacy-note",
                tags$i(
                  class = "fas fa-shield-halved privacy-icon",
                  role = "presentation",
                  `aria-hidden` = "true"
                ),
                tags$p(
                  if (ssh_login_enabled) {
                    tagList(
                      tags$strong("Privacy:"),
                      paste(
                        "Your credentials are used only to open this SSH session.",
                        "shinySCP does not save them to disk."
                      )
                    )
                  } else {
                    tagList(
                      tags$strong("Demo mode:"),
                      paste(
                        "This public demo does not accept or store credentials.",
                        "Select Try Demo to explore the sample filesystem."
                      )
                    )
                  }
                )
              ),
              htmltools::tagQuery(
                textInput("username", label = "Username", value = "")
              )$find("#username")$addAttrs(
                autocomplete = "username",
                required = if (ssh_login_enabled) NA else NULL,
                disabled = if (ssh_login_enabled) NULL else NA,
                `data-demo-disabled` = if (ssh_login_enabled) NULL else "true",
                `aria-describedby` = if (ssh_login_enabled) {
                  "login-guidance login-errors"
                } else {
                  "login-guidance"
                }
              )$allTags(),
              htmltools::tagQuery(
                textInput("server", label = "Server", value = "")
              )$find("#server")$addAttrs(
                required = if (ssh_login_enabled) NA else NULL,
                disabled = if (ssh_login_enabled) NULL else NA,
                `data-demo-disabled` = if (ssh_login_enabled) NULL else "true",
                `aria-describedby` = if (ssh_login_enabled) {
                  "login-guidance login-errors"
                } else {
                  "login-guidance"
                }
              )$allTags(),
              htmltools::tagQuery(
                passwordInput("password", label = "Password", value = "")
              )$find("#password")$addAttrs(
                autocomplete = "current-password",
                required = if (ssh_login_enabled) NA else NULL,
                disabled = if (ssh_login_enabled) NULL else NA,
                `data-demo-disabled` = if (ssh_login_enabled) NULL else "true",
                `aria-describedby` = if (ssh_login_enabled) {
                  "login-guidance login-errors"
                } else {
                  "login-guidance"
                }
              )$allTags(),
              tags$div(
                id = "login-errors",
                class = "login-errors",
                role = "alert",
                `aria-live` = "assertive",
                hidden = NA
              ),
              div(
                class = "login-actions",
                htmltools::tagAppendAttributes(
                  actionButton(
                    "submit",
                    "Login",
                    class = if (ssh_login_enabled) {
                      "btn-primary"
                    } else {
                      "btn-default"
                    }
                  ),
                  disabled = if (ssh_login_enabled) NULL else NA,
                  `data-demo-disabled` = if (ssh_login_enabled) NULL else "true"
                ),
                actionButton(
                  "try_demo",
                  "Try Demo",
                  icon = icon("folder-open"),
                  class = if (ssh_login_enabled) "btn-default" else "btn-primary"
                ),
                htmltools::tagAppendAttributes(
                  actionButton("reset", "Clear", class = "btn-default"),
                  disabled = if (ssh_login_enabled) NULL else NA,
                  `data-demo-disabled` = if (ssh_login_enabled) NULL else "true"
                )
              ),
              tags$div(
                id = "connection-status",
                class = "connection-status initial",
                textOutput("session")
              )
            )
          ),
          file_browser_ui("browser")
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
          tags$div(class = "container-fluid", includeMarkdown(about_path))
        ),
        column(3)
      )
    )
  )

  # Keep the profile link inside the collapsible navigation region so it remains
  # aligned at the right on wide screens and joins the menu on narrow screens.
  github_navigation <- tags$ul(
    class = "nav navbar-nav navbar-right",
    tags$li(
      tags$a(
        href = "https://github.com/sdhutchins",
        class = "navbar-github-link",
        `aria-label` = "Shaurita D. Hutchins on GitHub",
        title = "GitHub profile",
        tags$i(
          class = "fab fa-github",
          role = "presentation",
          `aria-hidden` = "true"
        )
      )
    )
  )
  ui <- htmltools::tagQuery(ui)$find(".navbar-collapse")$append(
    github_navigation
  )$allTags()

  # navbarPage creates the document body and tab container. Add the skip link at
  # the beginning of the body so keyboard users encounter it before navigation.
  ui <- htmltools::tagQuery(ui)$find(".tab-content")$addAttrs(
    id = "main-content",
    role = "main",
    tabindex = "-1"
  )$allTags()
  body_index <- which(vapply(
    ui,
    function(tag) inherits(tag, "shiny.tag") && identical(tag$name, "body"),
    logical(1)
  ))
  skip_link <- tags$a(
    class = "skip-link",
    href = "#main-content",
    "Skip to main content"
  )
  ui[[body_index]]$children <- c(list(skip_link), ui[[body_index]]$children)
  attr(ui, "lang") <- "en"

  ui
}

shiny_scp_server <- function(
  input,
  output,
  session,
  ssh_login_enabled = ssh_login_is_enabled()
) {
  stopifnot(
    is.logical(ssh_login_enabled),
    length(ssh_login_enabled) == 1L,
    !is.na(ssh_login_enabled)
  )

  x <- reactiveValues(
    username = NULL,
    server = NULL,
    ssh_session = NULL,
    connection_mode = NULL,
    is_connected = FALSE
  )

  browser <- file_browser_server(
    "browser",
    ssh_session = reactive(x$ssh_session),
    is_connected = reactive(x$is_connected),
    username = reactive(x$username),
    server_name = reactive(x$server),
    list_directory = list_app_directory
  )

  observe({
    if (x$is_connected) {
      shinyjs::removeClass(selector = ".main-container", class = "login-view")
      shinyjs::hide("login-panel-column")
      shinyjs::addClass(selector = "#connection-status", class = "connected")
      shinyjs::removeClass(selector = "#connection-status", class = "disconnected")
      shinyjs::removeClass(selector = "#connection-status", class = "initial")
    } else if (is.null(x$username) && is.null(x$server)) {
      shinyjs::addClass(selector = ".main-container", class = "login-view")
      shinyjs::show("login-panel-column")
      shinyjs::removeClass(selector = "#connection-status", class = "connected")
      shinyjs::removeClass(selector = "#connection-status", class = "disconnected")
      shinyjs::addClass(selector = "#connection-status", class = "initial")
    } else {
      shinyjs::addClass(selector = ".main-container", class = "login-view")
      shinyjs::show("login-panel-column")
      shinyjs::removeClass(selector = "#connection-status", class = "connected")
      shinyjs::removeClass(selector = "#connection-status", class = "initial")
      shinyjs::addClass(selector = "#connection-status", class = "disconnected")
    }
  })

  observeEvent(input$submit, {
    if (!ssh_login_enabled) {
      return()
    }

    # The browser starts the loading state immediately. Clear it and remove the
    # password from the browser after this observer finishes for any reason.
    on.exit(
      {
        session$sendCustomMessage(
          "set-login-loading",
          list(active = FALSE)
        )
        updateTextInput(session, "password", value = "")
      },
      add = TRUE
    )

    validation <- validate_ssh_credentials(
      username = input$username,
      server = input$server,
      password = input$password
    )

    if (!validation$is_valid) {
      session$sendCustomMessage(
        "set-login-validation",
        list(
          fields = validation$fields,
          message = validation$message
        )
      )
      return()
    }

    session$sendCustomMessage(
      "set-login-validation",
      list(fields = character(), message = "")
    )

    credentials <- validation$credentials
    validation <- NULL
    x$username <- credentials$username
    x$server <- credentials$server

    tryCatch(
      {
        x$ssh_session <- connect(
          credentials$username,
          credentials$server,
          credentials$password
        )
        # Remove the local reference as soon as the SSH client has consumed it.
        credentials$password <- NULL
        x$connection_mode <- "ssh"
        x$is_connected <- TRUE

        showModal(modalDialog(
          title = "Connection Successful",
          paste("Successfully connected to", x$server, "as", x$username),
          footer = modalButton("OK"),
          easyClose = TRUE
        ))
      },
      error = function(condition) {
        x$ssh_session <- NULL
        x$connection_mode <- NULL
        x$is_connected <- FALSE
        showModal(modalDialog(
          title = "Connection Failed",
          paste("Failed to connect:", condition$message),
          footer = modalButton("OK"),
          easyClose = TRUE
        ))
      }
    )
  })

  observeEvent(input$try_demo, {
    session$sendCustomMessage(
      "set-login-validation",
      list(fields = character(), message = "")
    )
    updateTextInput(session, "password", value = "")

    x$username <- "demo-user"
    x$server <- "Demo filesystem"
    x$ssh_session <- new_demo_session()
    x$connection_mode <- "demo"
    x$is_connected <- TRUE
  })

  observeEvent(browser$disconnect(), {
    if (!is.null(x$ssh_session)) {
      if (identical(x$connection_mode, "ssh")) {
        ssh::ssh_disconnect(x$ssh_session)
      }
      x$ssh_session <- NULL
      x$connection_mode <- NULL
      x$is_connected <- FALSE

      showModal(modalDialog(
        title = "Disconnected",
        "Successfully disconnected from server",
        footer = modalButton("OK"),
        easyClose = TRUE
      ))

      updateTextInput(session, "username", value = "")
      updateTextInput(session, "server", value = "")
      updateTextInput(session, "password", value = "")
    }
  })

  observeEvent(input$reset, {
    updateTextInput(session, "username", value = "")
    updateTextInput(session, "server", value = "")
    updateTextInput(session, "password", value = "")
    session$sendCustomMessage(
      "set-login-validation",
      list(fields = character(), message = "")
    )
  })

  output$session <- renderText({
    if (is.null(x$username) && is.null(x$server)) {
      if (ssh_login_enabled) {
        "Please enter your credentials to connect"
      } else {
        "Select Try Demo to explore the sample filesystem"
      }
    } else if (x$is_connected) {
      sprintf("Connected to %s as %s", x$server, x$username)
    } else {
      "Not connected"
    }
  })

  session$onSessionEnded(function() {
    ssh_session <- isolate(x$ssh_session)
    connection_mode <- isolate(x$connection_mode)
    if (!is.null(ssh_session) && identical(connection_mode, "ssh")) {
      ssh::ssh_disconnect(ssh_session)
    }
  })
}

#' Run shinySCP
#'
#' Start the shinySCP demonstration application.
#'
#' @param ... Additional arguments passed to [shiny::shinyApp()].
#'
#' @return A Shiny application object.
#' @export
shiny_scp_app <- function(...) {
  ssh_login_enabled <- ssh_login_is_enabled()

  shiny::shinyApp(
    ui = shiny_scp_ui(ssh_login_enabled),
    server = function(input, output, session) {
      shiny_scp_server(
        input = input,
        output = output,
        session = session,
        ssh_login_enabled = ssh_login_enabled
      )
    },
    ...
  )
}
