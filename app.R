# Load the current source checkout so Positron and deployment services run the
# same package code exercised by the development tests.
pkgload::load_all(".", export_all = FALSE, helpers = FALSE)

shiny_scp_app()
