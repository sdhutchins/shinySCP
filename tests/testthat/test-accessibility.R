read_color_property <- function(stylesheet, property_name) {
  pattern <- sprintf("--%s: (#[0-9a-fA-F]{6})", property_name)
  match <- regexec(pattern, stylesheet)
  captures <- regmatches(stylesheet, match)[[1L]]

  expect_length(captures, 2L)
  captures[2L]
}


relative_luminance <- function(hex_color) {
  channels <- strtoi(
    substring(hex_color, c(2, 4, 6), c(3, 5, 7)),
    base = 16L
  ) / 255
  adjusted_channels <- ifelse(
    channels <= 0.04045,
    channels / 12.92,
    ((channels + 0.055) / 1.055)^2.4
  )

  sum(adjusted_channels * c(0.2126, 0.7152, 0.0722))
}


contrast_ratio <- function(first_color, second_color) {
  luminance <- sort(c(
    relative_luminance(first_color),
    relative_luminance(second_color)
  ))

  (luminance[2L] + 0.05) / (luminance[1L] + 0.05)
}


test_that("the app exposes accessible page and login semantics", {
  rendered_ui <- as.character(shiny_scp_ui(ssh_login_enabled = FALSE))

  expect_identical(
    attr(shiny_scp_ui(ssh_login_enabled = FALSE), "lang"),
    "en"
  )
  expect_length(
    regmatches(
      rendered_ui,
      gregexpr('class="skip-link"', rendered_ui, fixed = TRUE)
    )[[1L]],
    1L
  )
  expect_match(rendered_ui, 'id="main-content" role="main"', fixed = TRUE)
  expect_match(rendered_ui, 'autocomplete="username"', fixed = TRUE)
  expect_match(rendered_ui, 'autocomplete="current-password"', fixed = TRUE)
  expect_match(rendered_ui, 'aria-describedby="login-guidance"', fixed = TRUE)
  expect_match(
    rendered_ui,
    'aria-label="Go to parent directory"',
    fixed = TRUE
  )
  expect_match(rendered_ui, 'class="login-privacy-note"', fixed = TRUE)
  expect_match(rendered_ui, 'id="try_demo"', fixed = TRUE)
  expect_match(rendered_ui, '>Try Demo</span>', fixed = TRUE)
  expect_match(
    rendered_ui,
    "This public demo does not accept or store credentials.",
    fixed = TRUE
  )
})


test_that("demo-only login controls expose native disabled states", {
  rendered_ui <- as.character(shiny_scp_ui(ssh_login_enabled = FALSE))
  control_ids <- c("username", "server", "password", "submit", "reset")

  for (control_id in control_ids) {
    control_pattern <- sprintf('<(?:input|button)[^>]+id="%s"[^>]*>', control_id)
    control_tag <- regmatches(rendered_ui, regexpr(control_pattern, rendered_ui))
    expect_match(control_tag, "(?:^| )disabled(?: |>|/)")
  }

  demo_button <- regmatches(
    rendered_ui,
    regexpr('<button[^>]+id="try_demo"[^>]*>', rendered_ui)
  )
  expect_false(grepl("disabled", demo_button, fixed = TRUE))
})


test_that("trusted mode enables SSH login controls", {
  rendered_ui <- as.character(shiny_scp_ui(ssh_login_enabled = TRUE))
  username_input <- regmatches(
    rendered_ui,
    regexpr('<input id="username"[^>]*>', rendered_ui)
  )

  expect_false(grepl("disabled", username_input, fixed = TRUE))
  expect_match(
    rendered_ui,
    "Your credentials are used only to open this SSH session.",
    fixed = TRUE
  )
})


test_that("developer links and decorative icons are labelled correctly", {
  rendered_ui <- as.character(shiny_scp_ui())
  footer_html <- regmatches(
    rendered_ui,
    regexpr("<footer[^>]*>.*?</footer>", rendered_ui)
  )

  expect_match(rendered_ui, 'class="app-footer"', fixed = TRUE)
  expect_match(
    rendered_ui,
    "shinySCP was created for demo purposes.",
    fixed = TRUE
  )
  expect_match(
    rendered_ui,
    'href="https://www.shauritahutchins.com/">Shaurita D. Hutchins.</a>',
    fixed = TRUE
  )
  expect_match(
    rendered_ui,
    'aria-label="Shaurita D. Hutchins on GitHub"',
    fixed = TRUE
  )
  expect_match(rendered_ui, 'class="fab fa-github"', fixed = TRUE)
  expect_false(grepl("fa-github", footer_html, fixed = TRUE))
})


test_that("the application palette meets its contrast thresholds", {
  stylesheet_path <- system.file(
    "app/www/styles.css",
    package = "shinySCP"
  )
  stylesheet <- paste(readLines(stylesheet_path, warn = FALSE), collapse = "\n")
  page_color <- read_color_property(stylesheet, "color-page")
  text_properties <- c(
    "color-heading",
    "color-text",
    "color-primary",
    "color-primary-hover",
    "color-focus",
    "color-error",
    "color-danger",
    "color-danger-hover"
  )
  non_text_properties <- c(
    "color-error-border",
    "color-folder",
    "color-file",
    "color-success"
  )

  for (property_name in text_properties) {
    expect_gte(
      contrast_ratio(
        read_color_property(stylesheet, property_name),
        page_color
      ),
      4.5,
      label = property_name
    )
  }
  for (property_name in non_text_properties) {
    expect_gte(
      contrast_ratio(
        read_color_property(stylesheet, property_name),
        page_color
      ),
      3,
      label = property_name
    )
  }

  expect_gte(
    contrast_ratio(
      read_color_property(stylesheet, "color-connected-text"),
      read_color_property(stylesheet, "color-connected-bg")
    ),
    4.5
  )
  expect_gte(
    contrast_ratio(
      read_color_property(stylesheet, "color-disconnected-text"),
      read_color_property(stylesheet, "color-disconnected-bg")
    ),
    4.5
  )
})


test_that("the stylesheet retains responsive and font rules", {
  stylesheet_path <- system.file(
    "app/www/styles.css",
    package = "shinySCP"
  )
  stylesheet <- paste(readLines(stylesheet_path, warn = FALSE), collapse = "\n")

  expect_match(stylesheet, "--font-family-ui: Inter", fixed = TRUE)
  expect_match(stylesheet, ".form-control::placeholder", fixed = TRUE)
  expect_match(stylesheet, "body.bslib-page-navbar", fixed = TRUE)
  expect_match(stylesheet, ".browser-panel-column", fixed = TRUE)
  expect_false(grepl("min-height: calc(100vh - 52px)", stylesheet, fixed = TRUE))
})
