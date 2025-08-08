library(ssh)


connect <- function(username, server, password) {
  if (is.null(password)) {
    tryCatch(ssh_connect(paste0(username, "@", server), verbose = 0), 
                        error = function(c) {
                          c$message <-  paste0("Unsuccessful connection.")
                          stop(c)})
  } else {
    tryCatch(ssh_connect(paste0(username, "@", server), passwd = password, verbose = 0), 
             error = function(c) {
               c$message <-  paste0("Unsuccessful connection.")
               stop(c)})
  }
}

get_remote_pwd <- function(session) {
  result <- ssh_exec_internal(session, command = "pwd")
  if (result$status == 0) {
    pwd <- trimws(rawToChar(result$stdout))
    return(pwd)
  } else {
    stop("Failed to get current directory")
  }
}

list_remote_dir <- function(session, path = NULL) {
  # If no path specified or path is "~", get current directory first
  if (is.null(path) || path == "~" || path == "") {
    path <- get_remote_pwd(session)
  }
  
  # Clean up the path
  path <- gsub("^~", "", path)  # Remove leading tilde
  path <- gsub("//", "/", path)  # Clean up double slashes
  
  # Execute ls command with details (-la) and handle errors
  cmd <- sprintf("ls -la %s", shQuote(path))
  result <- ssh_exec_internal(session, command = cmd)
  
  if (result$status == 0) {
    # Convert raw output to character and split into lines
    files <- strsplit(rawToChar(result$stdout), "\n")[[1]]
    # Remove empty lines and format output
    files <- files[nzchar(files)]
    return(files)
  } else {
    error_msg <- rawToChar(result$stderr)
    stop(paste("Error listing directory:", error_msg))
  }
}
