library(ssh)


connect <- function(username, server, password) {
  if (is.null(password)) {
    session <- ssh_connect(paste0(username, "@", server), verbose = 4)
  } else {
    session <- ssh_connect(paste0(username, "@", server), passwd = password, verbose = 4)
  }
  return(session)
}
