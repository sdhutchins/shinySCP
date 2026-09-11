validate_ssh_credentials <- function(username, server, password) {
  credentials <- list(
    username = username,
    server = server,
    password = password
  )
  credential_labels <- c(
    username = "username",
    server = "server",
    password = "password"
  )

  is_non_empty_scalar <- function(value, trim_whitespace = TRUE) {
    if (!is.character(value) || length(value) != 1L || is.na(value)) {
      return(FALSE)
    }

    checked_value <- if (trim_whitespace) trimws(value) else value
    nzchar(checked_value)
  }

  valid_fields <- c(
    username = is_non_empty_scalar(credentials$username),
    server = is_non_empty_scalar(credentials$server),
    # Whitespace can be part of a password, so do not normalize it.
    password = is_non_empty_scalar(
      credentials$password,
      trim_whitespace = FALSE
    )
  )
  missing_fields <- names(valid_fields)[!valid_fields]

  if (length(missing_fields) == 0L) {
    return(list(
      is_valid = TRUE,
      fields = character(),
      message = "",
      credentials = list(
        username = trimws(credentials$username),
        server = trimws(credentials$server),
        password = credentials$password
      )
    ))
  }

  missing_labels <- credential_labels[missing_fields]
  field_list <- switch(as.character(length(missing_labels)),
    "1" = missing_labels,
    "2" = paste(missing_labels, collapse = " and "),
    paste0(
      paste(utils::head(missing_labels, -1L), collapse = ", "),
      ", and ",
      utils::tail(missing_labels, 1L)
    )
  )

  list(
    is_valid = FALSE,
    fields = missing_fields,
    message = sprintf("Enter your %s.", field_list),
    credentials = NULL
  )
}


connect <- function(username, server, password) {
  if (is.null(password)) {
    tryCatch(
      ssh::ssh_connect(paste0(username, "@", server), verbose = 0),
      error = function(condition) {
        condition$message <- "Unsuccessful connection."
        stop(condition)
      }
    )
  } else {
    tryCatch(
      ssh::ssh_connect(
        paste0(username, "@", server),
        passwd = password,
        verbose = 0
      ),
      error = function(condition) {
        condition$message <- "Unsuccessful connection."
        stop(condition)
      }
    )
  }
}

run_remote_command <- function(
  session,
  command,
  execute_remote = ssh::ssh_exec_internal
) {
  result <- execute_remote(session, command = command)

  if (!identical(result$status, 0L) && !identical(result$status, 0)) {
    error_message <- rawToChar(result$stderr)
    if (!nzchar(error_message)) {
      error_message <- "The remote command failed without an error message."
    }
    stop(trimws(error_message), call. = FALSE)
  }

  # Remove only line endings added by the command. Spaces can be valid in paths.
  sub("[\r\n]+$", "", rawToChar(result$stdout))
}

get_remote_home <- function(
  session,
  execute_remote = ssh::ssh_exec_internal
) {
  home_directory <- run_remote_command(
    session,
    "printf '%s\\n' \"$HOME\"",
    execute_remote
  )

  if (!nzchar(home_directory)) {
    stop("The remote server did not report a home directory.", call. = FALSE)
  }

  home_directory
}

get_remote_pwd <- function(
  session,
  execute_remote = ssh::ssh_exec_internal
) {
  working_directory <- run_remote_command(
    session,
    "pwd -P",
    execute_remote
  )

  if (!nzchar(working_directory)) {
    stop("The remote server did not report a working directory.", call. = FALSE)
  }

  working_directory
}

expand_remote_home <- function(path, home_directory) {
  if (identical(path, "~")) {
    return(home_directory)
  }

  if (startsWith(path, "~/")) {
    return(paste0(home_directory, substring(path, 2L)))
  }

  path
}

resolve_remote_directory <- function(
  session,
  path,
  base_directory = NULL,
  execute_remote = ssh::ssh_exec_internal
) {
  if (length(path) != 1L || is.na(path)) {
    stop("The remote path must be one non-missing value.", call. = FALSE)
  }

  requested_path <- path
  if (!nzchar(trimws(requested_path))) {
    requested_path <- if (is.null(base_directory)) "~" else base_directory
  }

  if (identical(requested_path, "~") || startsWith(requested_path, "~/")) {
    home_directory <- get_remote_home(session, execute_remote)
    requested_path <- expand_remote_home(requested_path, home_directory)
  } else if (!startsWith(requested_path, "/")) {
    if (is.null(base_directory)) {
      base_directory <- get_remote_pwd(session, execute_remote)
    }
    requested_path <- paste0(sub("/$", "", base_directory), "/", requested_path)
  }

  command <- sprintf(
    "cd -- %s 2>/dev/null && pwd -P",
    shQuote(requested_path)
  )
  resolved_directory <- run_remote_command(session, command, execute_remote)

  if (!nzchar(resolved_directory)) {
    stop("The remote directory could not be resolved.", call. = FALSE)
  }

  resolved_directory
}

parse_remote_entries <- function(output, directory) {
  empty_entries <- data.frame(
    name = character(),
    path = character(),
    type = character(),
    size = numeric(),
    modified = as.POSIXct(character(), tz = "UTC"),
    stringsAsFactors = FALSE
  )

  if (!nzchar(output)) {
    return(empty_entries)
  }

  record_separator <- "\036"
  field_separator <- "\037"
  records <- strsplit(output, record_separator, fixed = TRUE)[[1L]]
  records <- records[nzchar(records)]

  parsed_records <- lapply(records, function(record) {
    fields <- strsplit(record, field_separator, fixed = TRUE)[[1L]]
    if (length(fields) != 4L) {
      stop(
        "The remote directory listing contained an unsupported filename.",
        call. = FALSE
      )
    }
    fields
  })

  entry_matrix <- do.call(rbind, parsed_records)
  type_codes <- entry_matrix[, 2L]
  entry_types <- unname(c(
    d = "Directory",
    f = "File",
    l = "Symbolic link"
  )[type_codes])
  entry_types[is.na(entry_types)] <- "Other"

  directory_prefix <- if (identical(directory, "/")) {
    "/"
  } else {
    paste0(directory, "/")
  }
  modified_seconds <- suppressWarnings(as.numeric(entry_matrix[, 4L]))

  entries <- data.frame(
    name = entry_matrix[, 1L],
    path = paste0(directory_prefix, entry_matrix[, 1L]),
    type = entry_types,
    size = suppressWarnings(as.numeric(entry_matrix[, 3L])),
    modified = as.POSIXct(
      modified_seconds,
      origin = "1970-01-01",
      tz = "UTC"
    ),
    stringsAsFactors = FALSE
  )

  entries[order(entries$type != "Directory", tolower(entries$name)), ]
}

list_remote_dir <- function(
  session,
  path = "~",
  base_directory = NULL,
  execute_remote = ssh::ssh_exec_internal
) {
  resolved_directory <- resolve_remote_directory(
    session = session,
    path = path,
    base_directory = base_directory,
    execute_remote = execute_remote
  )

  quoted_directory <- shQuote(resolved_directory)
  # GNU and BSD stat use different flags, so select the supported form once.
  command <- paste(
    "if stat -c '%s' / >/dev/null 2>&1; then",
    "file_size() { stat -c '%s' -- \"$1\"; };",
    "file_modified() { stat -c '%Y' -- \"$1\"; };",
    "else",
    "file_size() { stat -f '%z' \"$1\"; };",
    "file_modified() { stat -f '%m' \"$1\"; };",
    "fi;",
    paste0(
      "for entry in ",
      quoted_directory,
      "/.[!.]* ",
      quoted_directory,
      "/..?* ",
      quoted_directory,
      "/*; do"
    ),
    "[ -e \"$entry\" ] || [ -L \"$entry\" ] || continue;",
    "entry_name=${entry##*/};",
    paste(
      "if [ -d \"$entry\" ]; then entry_type=d;",
      "elif [ -f \"$entry\" ]; then entry_type=f;",
      "elif [ -L \"$entry\" ]; then entry_type=l;",
      "else entry_type=o; fi;"
    ),
    "entry_size=$(file_size \"$entry\") || entry_size=;",
    "entry_modified=$(file_modified \"$entry\") || entry_modified=;",
    paste0(
      "printf '%s\\037%s\\037%s\\037%s\\036' ",
      "\"$entry_name\" \"$entry_type\" ",
      "\"$entry_size\" \"$entry_modified\";"
    ),
    "done"
  )
  listing_output <- run_remote_command(session, command, execute_remote)

  list(
    directory = resolved_directory,
    entries = parse_remote_entries(listing_output, resolved_directory)
  )
}

format_file_size <- function(size, type) {
  if (identical(type, "Directory") || is.na(size)) {
    return("")
  }

  units <- c("B", "KB", "MB", "GB", "TB")
  unit_index <- min(
    floor(log(max(size, 1), base = 1024)) + 1L,
    length(units)
  )
  scaled_size <- size / 1024^(unit_index - 1L)

  if (unit_index == 1L) {
    return(sprintf("%d %s", as.integer(size), units[unit_index]))
  }

  sprintf("%.1f %s", scaled_size, units[unit_index])
}

build_remote_breadcrumbs <- function(directory) {
  if (
    length(directory) != 1L ||
      is.na(directory) ||
      !startsWith(directory, "/")
  ) {
    stop("The breadcrumb directory must be an absolute path.", call. = FALSE)
  }

  path_parts <- strsplit(sub("^/", "", directory), "/", fixed = TRUE)[[1L]]
  path_parts <- path_parts[nzchar(path_parts)]

  if (length(path_parts) == 0L) {
    return(data.frame(label = "/", path = "/", stringsAsFactors = FALSE))
  }

  breadcrumb_paths <- vapply(
    seq_along(path_parts),
    function(part_index) {
      paste0("/", paste(path_parts[seq_len(part_index)], collapse = "/"))
    },
    character(1)
  )

  data.frame(
    label = c("/", path_parts),
    path = c("/", breadcrumb_paths),
    stringsAsFactors = FALSE
  )
}

filter_remote_entries <- function(entries, show_hidden = FALSE, query = "") {
  visible_entries <- entries

  if (!isTRUE(show_hidden)) {
    visible_entries <- visible_entries[!startsWith(visible_entries$name, "."), ]
  }

  normalized_query <- trimws(query)
  if (nzchar(normalized_query)) {
    matches_query <- grepl(
      tolower(normalized_query),
      tolower(visible_entries$name),
      fixed = TRUE
    )
    visible_entries <- visible_entries[matches_query, ]
  }

  rownames(visible_entries) <- NULL
  visible_entries
}
