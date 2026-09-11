with_ssh_login_setting <- function(value, code) {
  variable_name <- "SHINYSCP_ENABLE_SSH_LOGIN"
  previous_value <- Sys.getenv(variable_name, unset = NA_character_)
  on.exit(
    if (is.na(previous_value)) {
      Sys.unsetenv(variable_name)
    } else {
      do.call(Sys.setenv, stats::setNames(list(previous_value), variable_name))
    },
    add = TRUE
  )
  do.call(Sys.setenv, stats::setNames(list(value), variable_name))
  force(code)
}


test_that("demo sessions identify the local browsing backend", {
  demo_session <- new_demo_session()

  expect_true(is_demo_session(demo_session))
  expect_false(is_demo_session(list(id = "ssh-session")))
})


test_that("SSH login requires an explicit true setting", {
  expect_false(ssh_login_is_enabled("false"))
  expect_false(ssh_login_is_enabled("1"))
  expect_false(ssh_login_is_enabled("yes"))
  expect_false(ssh_login_is_enabled(NA_character_))
  expect_true(ssh_login_is_enabled(" TRUE "))
})


test_that("the demo home is realistic and hides private entries by default", {
  listing <- list_demo_directory(new_demo_session(), "~")
  visible_entries <- filter_remote_entries(listing$entries)

  expect_identical(listing$directory, "/home/demo-user")
  expect_equal(nrow(listing$entries), 13L)
  expect_equal(nrow(visible_entries), 10L)
  expect_setequal(
    listing$entries$name[startsWith(listing$entries$name, ".")],
    c(".config", ".ssh", ".Rhistory")
  )
  expect_true(all(listing$entries$type %in% c("Directory", "File")))
})


test_that("demo paths support home, relative, and parent navigation", {
  demo_session <- new_demo_session()

  project_listing <- list_demo_directory(
    demo_session,
    "projects",
    "/home/demo-user"
  )
  parent_listing <- list_demo_directory(
    demo_session,
    "..",
    "/home/demo-user/projects"
  )

  expect_identical(project_listing$directory, "/home/demo-user/projects")
  expect_true("rnaseq-pipeline" %in% project_listing$entries$name)
  expect_identical(parent_listing$directory, "/home/demo-user")
})


test_that("the demo backend rejects unknown directories", {
  expect_error(
    list_demo_directory(new_demo_session(), "/home/demo-user/not-real"),
    "demo directory does not exist"
  )
})


test_that("the main server enters demo mode without SSH credentials", {
  with_ssh_login_setting("false", {
    shiny::testServer(shiny_scp_server, {
      session$setInputs(try_demo = 1)
      session$flushReact()

      expect_true(x$is_connected)
      expect_identical(x$connection_mode, "demo")
      expect_identical(x$username, "demo-user")
      expect_true(is_demo_session(x$ssh_session))
      expect_identical(browser$current_directory(), "/home/demo-user")

      do.call(
        session$setInputs,
        stats::setNames(list(1), "browser-disconnect")
      )
      session$flushReact()

      expect_false(x$is_connected)
      expect_null(x$connection_mode)
      expect_null(x$ssh_session)
    })
  })
})


test_that("demo-only server ignores forged SSH login input", {
  with_ssh_login_setting("false", {
    shiny::testServer(shiny_scp_server, {
      session$setInputs(
        username = "test-user",
        server = "example.org",
        password = "not-a-real-password",
        submit = 1
      )
      session$flushReact()

      expect_false(x$is_connected)
      expect_null(x$username)
      expect_null(x$server)
      expect_null(x$ssh_session)
    })
  })
})


test_that("the demo manifest contains no personal filesystem identifiers", {
  manifest <- read_demo_manifest()
  manifest_text <- paste(unlist(manifest), collapse = " ")

  expect_false(grepl("shutchin", manifest_text, ignore.case = TRUE))
})
