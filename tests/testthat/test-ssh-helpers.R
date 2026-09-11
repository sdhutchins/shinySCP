make_result <- function(stdout = "", stderr = "", status = 0L) {
  list(
    stdout = charToRaw(stdout),
    stderr = charToRaw(stderr),
    status = status
  )
}


path_executor <- function(session, command) {
  if (identical(command, "printf '%s\\n' \"$HOME\"")) {
    return(make_result("/home/alice\n"))
  }
  if (identical(command, "pwd -P")) {
    return(make_result("/home/alice\n"))
  }
  if (grepl("missing", command, fixed = TRUE)) {
    return(make_result(stderr = "Directory not found\n", status = 1L))
  }
  if (grepl("/home/alice/data", command, fixed = TRUE)) {
    return(make_result("/home/alice/data\n"))
  }
  if (grepl("/scratch/project/results", command, fixed = TRUE)) {
    return(make_result("/scratch/project/results\n"))
  }
  if (grepl("/scratch/project/..", command, fixed = TRUE)) {
    return(make_result("/scratch\n"))
  }
  if (grepl("/shared", command, fixed = TRUE)) {
    return(make_result("/shared\n"))
  }

  stop(sprintf("Unexpected test command: %s", command))
}


test_that("SSH credentials are validated and normalized", {
  valid_credentials <- validate_ssh_credentials(
    username = " alice ",
    server = " example.org ",
    password = " secret phrase "
  )
  missing_credentials <- validate_ssh_credentials(
    username = NA_character_,
    server = "   ",
    password = NULL
  )

  expect_true(valid_credentials$is_valid)
  expect_identical(valid_credentials$credentials$username, "alice")
  expect_identical(valid_credentials$credentials$server, "example.org")
  expect_identical(
    valid_credentials$credentials$password,
    " secret phrase "
  )
  expect_false(missing_credentials$is_valid)
  expect_identical(
    missing_credentials$fields,
    c("username", "server", "password")
  )
  expect_identical(
    missing_credentials$message,
    "Enter your username, server, and password."
  )
  expect_null(missing_credentials$credentials)
})


test_that("remote directory paths resolve from supported starting points", {
  expect_identical(expand_remote_home("~", "/home/alice"), "/home/alice")
  expect_identical(
    expand_remote_home("~/data", "/home/alice"),
    "/home/alice/data"
  )
  expect_identical(
    resolve_remote_directory(
      NULL,
      "~/data",
      execute_remote = path_executor
    ),
    "/home/alice/data"
  )
  expect_identical(
    resolve_remote_directory(
      NULL,
      "results",
      base_directory = "/scratch/project",
      execute_remote = path_executor
    ),
    "/scratch/project/results"
  )
  expect_identical(
    resolve_remote_directory(
      NULL,
      "..",
      base_directory = "/scratch/project",
      execute_remote = path_executor
    ),
    "/scratch"
  )
})


test_that("directory resolution reports remote command failures", {
  expect_error(
    resolve_remote_directory(
      NULL,
      "/missing",
      execute_remote = path_executor
    ),
    "Directory not found",
    fixed = TRUE
  )
})


test_that("remote listing output is parsed and sorted", {
  listing_output <- paste0(
    "analysis results.tsv\037f\0371536\0371788912000.0\036",
    "data\037d\0374096\0371788825600.0\036"
  )
  entries <- parse_remote_entries(listing_output, "/home/alice")

  expect_identical(entries$name, c("data", "analysis results.tsv"))
  expect_identical(entries$type, c("Directory", "File"))
  expect_identical(
    entries$path,
    c("/home/alice/data", "/home/alice/analysis results.tsv")
  )
  expect_identical(format_file_size(entries$size[1L], entries$type[1L]), "")
  expect_identical(
    format_file_size(entries$size[2L], entries$type[2L]),
    "1.5 KB"
  )
  expect_equal(nrow(parse_remote_entries("", "/home/alice")), 0L)
})


test_that("breadcrumbs represent every level of an absolute path", {
  breadcrumbs <- build_remote_breadcrumbs("/home/alice/projects")

  expect_identical(breadcrumbs$label, c("/", "home", "alice", "projects"))
  expect_identical(
    breadcrumbs$path,
    c("/", "/home", "/home/alice", "/home/alice/projects")
  )
  expect_identical(build_remote_breadcrumbs("/")$path, "/")
  expect_error(
    build_remote_breadcrumbs("relative/path"),
    "must be an absolute path",
    fixed = TRUE
  )
})


test_that("remote entries can be filtered by visibility and name", {
  entries <- data.frame(
    name = c(".ssh", "analysis.R", "data"),
    path = c("/home/alice/.ssh", "/home/alice/analysis.R", "/home/alice/data"),
    type = c("Directory", "File", "Directory"),
    size = c(4096, 512, 4096),
    modified = as.POSIXct(rep("2026-09-11 12:00:00", 3), tz = "UTC"),
    stringsAsFactors = FALSE
  )

  expect_identical(filter_remote_entries(entries)$name, c("analysis.R", "data"))
  expect_identical(
    filter_remote_entries(entries, show_hidden = TRUE)$name,
    c(".ssh", "analysis.R", "data")
  )
  expect_identical(
    filter_remote_entries(entries, query = "ANALYSIS")$name,
    "analysis.R"
  )
})


test_that("directory listing works with representative local files", {
  local_executor <- function(session, command) {
    stdout_file <- tempfile("shinySCP-stdout-")
    stderr_file <- tempfile("shinySCP-stderr-")
    on.exit(unlink(c(stdout_file, stderr_file)), add = TRUE)

    status <- system2(
      "sh",
      c("-c", shQuote(command)),
      stdout = stdout_file,
      stderr = stderr_file
    )

    list(
      stdout = readBin(
        stdout_file,
        what = "raw",
        n = file.info(stdout_file)$size
      ),
      stderr = readBin(
        stderr_file,
        what = "raw",
        n = file.info(stderr_file)$size
      ),
      status = status
    )
  }

  test_directory <- tempfile("shinySCP browser test ")
  dir.create(test_directory)
  on.exit(unlink(test_directory, recursive = TRUE), add = TRUE)
  dir.create(file.path(test_directory, "data"))
  writeLines(
    "representative file",
    file.path(test_directory, "analysis results.tsv")
  )
  resolved_test_directory <- normalizePath(test_directory)

  local_listing <- list_remote_dir(
    NULL,
    test_directory,
    execute_remote = local_executor
  )

  expect_identical(local_listing$directory, resolved_test_directory)
  expect_identical(
    local_listing$entries$name,
    c("data", "analysis results.tsv")
  )
  expect_identical(local_listing$entries$type, c("Directory", "File"))
})


test_that("directory listing reports permission failures", {
  permission_executor <- function(session, command) {
    if (startsWith(command, "cd --")) {
      return(make_result("/restricted\n"))
    }

    make_result(stderr = "Permission denied\n", status = 1L)
  }

  expect_error(
    list_remote_dir(
      NULL,
      "/restricted",
      execute_remote = permission_executor
    ),
    "Permission denied",
    fixed = TRUE
  )
})
