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
  expect_equal(sum(data$trees$frames), nrow(stand$rows))
  expect_equal(data$colour$column, c("net_mass_production_dt", "area_leaf"))

  ## The packed heights decode to the stand's, within 16-bit resolution.
  h <- data$rows$height
  q <- readBin(jsonlite::base64_dec(h$data), "integer", n = nrow(stand$rows),
               size = 2, signed = FALSE, endian = "little")
  expect_equal(h$min + q * h$scale, stand$rows$height, tolerance = 1e-4)

  expect_error(write_viewer(stand, file, colour_by = "no_such_column"), "Not in the stand")
  write_viewer(stand, file, standalone = FALSE)
  expect_false(grepl("<!doctype", paste(readLines(file), collapse = "\n"), fixed = TRUE))
})
