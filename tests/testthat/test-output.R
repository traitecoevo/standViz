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

test_that("each tree's rows sit together from its offset, one per frame", {
  skip_if_not_installed("plant")
  stand <- realise_stand(ff16_run(), side = 10, min_height = 2)
  data <- jsonlite::fromJSON(stand_json(stand))
  tr <- data$trees
  for (j in c(1, 7, length(tr$frames))) {
    i <- tr$offset[j] + seq_len(tr$frames[j])
    expect_true(all(stand$rows$tree[i] == j))
    expect_equal(stand$rows$frame[i] - 1, tr$birth[j] + seq_len(tr$frames[j]) - 1)
  }
  expect_equal(tr$offset[-1], cumsum(tr$frames)[-length(tr$frames)])
})

test_that("a stand with no tree tall enough still prints and writes", {
  skip_if_not_installed("plant")
  stand <- realise_stand(ff16_run(), side = 5, min_height = 1000)
  expect_equal(nrow(stand$trees), 0)
  expect_output(print(stand), "up to 0 at once")
})
