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
