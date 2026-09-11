new_demo_session <- function() {
  structure(
    list(id = "demo-filesystem"),
    class = "shinySCP_demo_session"
  )
}


is_demo_session <- function(session) {
  inherits(session, "shinySCP_demo_session")
}


normalize_demo_path <- function(path, base_directory = NULL) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    stop("The demo path must be one non-missing value.", call. = FALSE)
  }

  requested_path <- trimws(path)
  if (!nzchar(requested_path)) {
    requested_path <- if (is.null(base_directory)) "~" else base_directory
  }

  demo_home <- "/home/demo-user"
  if (identical(requested_path, "~")) {
    requested_path <- demo_home
  } else if (startsWith(requested_path, "~/")) {
    requested_path <- paste0(demo_home, substring(requested_path, 2L))
  } else if (!startsWith(requested_path, "/")) {
    starting_directory <- if (is.null(base_directory)) {
      demo_home
    } else {
      base_directory
    }
    requested_path <- paste0(sub("/$", "", starting_directory), "/", requested_path)
  }

  path_parts <- strsplit(requested_path, "/", fixed = TRUE)[[1L]]
  normalized_parts <- character()
  for (path_part in path_parts) {
    if (!nzchar(path_part) || identical(path_part, ".")) {
      next
    }
    if (identical(path_part, "..")) {
      normalized_parts <- utils::head(normalized_parts, -1L)
      next
    }
    normalized_parts <- c(normalized_parts, path_part)
  }

  if (length(normalized_parts) == 0L) {
    return("/")
  }
  paste0("/", paste(normalized_parts, collapse = "/"))
}


read_demo_manifest <- function() {
  manifest_path <- system.file(
    "app/demo/filesystem.csv",
    package = "shinySCP"
  )
  if (!nzchar(manifest_path)) {
    stop("The demo filesystem manifest could not be located.", call. = FALSE)
  }

  manifest <- utils::read.csv(
    manifest_path,
    na.strings = "",
    stringsAsFactors = FALSE
  )
  manifest$modified <- as.POSIXct(manifest$modified, tz = "UTC")
  manifest
}


list_demo_directory <- function(session, path = "~", base_directory = NULL) {
  if (!is_demo_session(session)) {
    stop("A demo session is required to browse the demo filesystem.", call. = FALSE)
  }

  manifest <- read_demo_manifest()
  resolved_directory <- normalize_demo_path(path, base_directory)
  known_directories <- unique(c(
    manifest$directory,
    manifest$path[manifest$type == "Directory"]
  ))

  if (!resolved_directory %in% known_directories) {
    stop("The demo directory does not exist.", call. = FALSE)
  }

  entries <- manifest[manifest$directory == resolved_directory, c(
    "name", "path", "type", "size", "modified"
  )]
  entries <- entries[order(entries$type != "Directory", tolower(entries$name)), ]
  rownames(entries) <- NULL

  list(directory = resolved_directory, entries = entries)
}


list_app_directory <- function(session, path = "~", base_directory = NULL) {
  if (is_demo_session(session)) {
    return(list_demo_directory(session, path, base_directory))
  }
  list_remote_dir(session, path, base_directory)
}
