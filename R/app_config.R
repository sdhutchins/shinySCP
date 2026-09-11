ssh_login_is_enabled <- function(
  value = Sys.getenv("SHINYSCP_ENABLE_SSH_LOGIN", unset = "false")
) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    identical(tolower(trimws(value)), "true")
}
