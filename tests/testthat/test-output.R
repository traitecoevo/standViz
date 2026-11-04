test_that("write_viewer embeds the stand in one page", {
  skip_if_not_installed("plant")
  stand <- realise_stand(ff16_run(), side = 10, min_height = 2)
  file <- withr::local_tempfile(fileext = ".html")
  write_viewer(stand, file, title = "Test stand")
  html <- paste(readLines(file), collapse = "\n")
  expect_false(grepl("__STAND_", html, fixed = TRUE))
  expect_match(html, "<title>Test stand</title>", fixed = TRUE)
  expect_match(html, "<!doctype html>", fixed = TRUE)

  json <- regmatches(html, regexpr('(?s)id="stand-data">.*?</script>', html, perl = TRUE))
  data <- jsonlite::fromJSON(sub("</script>$", "", sub('^id="stand-data">', "", json)))
  expect_equal(length(data$times), length(stand$times))
  expect_equal(length(data$trees$v), nrow(stand$trees))
  expect_equal(min(data$trees$birth), min(stand$trees$birth_frame) - 1)

  write_viewer(stand, file, standalone = FALSE)
  expect_false(grepl("<!doctype", paste(readLines(file), collapse = "\n"), fixed = TRUE))
})
