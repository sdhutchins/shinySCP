sample_entries <- data.frame(
  name = c(".ssh", "analysis.R", "data"),
  path = c("/home/alice/.ssh", "/home/alice/analysis.R", "/home/alice/data"),
  type = c("Directory", "File", "Directory"),
  size = c(4096, 512, 4096),
  modified = as.POSIXct(rep("2026-09-11 12:00:00", 3), tz = "UTC"),
  stringsAsFactors = FALSE
)


list_test_directory <- function(session, path, base_directory = NULL) {
  expect_identical(session$id, "test-session")
  list(
    directory = "/home/alice",
    entries = sample_entries
  )
}


test_that("the main server does not retain a password", {
  shiny::testServer(shiny_scp_server, {
    session_state <- shiny::isolate(shiny::reactiveValuesToList(x))
    expect_false("password" %in% names(session_state))
  })
})


test_that("the browser filters entries and reports connection identity", {
  test_ssh_session <- shiny::reactiveVal(list(id = "test-session"))
  test_is_connected <- shiny::reactiveVal(FALSE)
  test_username <- shiny::reactiveVal("alice")
  test_server_name <- shiny::reactiveVal("example.org")

  shiny::testServer(
    file_browser_server,
    args = list(
      ssh_session = test_ssh_session,
      is_connected = test_is_connected,
      username = test_username,
      server_name = test_server_name,
      list_directory = list_test_directory
    ),
    {
      test_is_connected(TRUE)
      session$flushReact()
      session$setInputs(show_hidden_files = FALSE, file_filter = "")

      expect_identical(visible_entries()$name, c("analysis.R", "data"))
      expect_identical(
        output$connected_identity,
        "Connected to example.org as alice"
      )
      expect_identical(
        output$listing_summary,
        "Showing 2 of 3 entries \u00b7 0 selected"
      )

      session$setInputs(file_filter = "analysis")
      expect_identical(visible_entries()$name, "analysis.R")
      expect_identical(
        output$listing_summary,
        "Showing 1 of 3 entries \u00b7 0 selected"
      )

      session$setInputs(show_hidden_files = TRUE, file_filter = "")
      expect_identical(
        visible_entries()$name,
        c(".ssh", "analysis.R", "data")
      )
    }
  )
})


test_that("the browser accepts only entries in its current listing", {
  test_ssh_session <- shiny::reactiveVal(list(id = "test-session"))
  test_is_connected <- shiny::reactiveVal(FALSE)
  test_username <- shiny::reactiveVal("alice")
  test_server_name <- shiny::reactiveVal("example.org")

  shiny::testServer(
    file_browser_server,
    args = list(
      ssh_session = test_ssh_session,
      is_connected = test_is_connected,
      username = test_username,
      server_name = test_server_name,
      list_directory = list_test_directory
    ),
    {
      test_is_connected(TRUE)
      session$flushReact()
      session$setInputs(
        show_hidden_files = FALSE,
        file_filter = "",
        selected_remote_path = "/home/alice/analysis.R"
      )
      expect_identical(
        browser_state$selected_path,
        "/home/alice/analysis.R"
      )

      session$setInputs(selected_remote_path = "/home/alice/not-listed.txt")
      expect_identical(
        browser_state$selected_path,
        "/home/alice/analysis.R"
      )
    }
  )
})
