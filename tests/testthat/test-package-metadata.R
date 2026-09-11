test_that("runtime Markdown dependencies are declared", {
  imports <- utils::packageDescription("shinySCP", fields = "Imports")
  imported_packages <- trimws(strsplit(
    gsub("\\s*\\([^)]*\\)", "", imports),
    ","
  )[[1L]])

  expect_true("markdown" %in% imported_packages)
})
