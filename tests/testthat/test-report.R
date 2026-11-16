## The report: its contents are computed in R and tested here without Quarto;
## one test renders a page where the Quarto CLI is installed.

test_that("an FF16 report holds every figure and the inputs that differ", {
  skip_if_not_installed("plant")
  res <- ff16_run()
  contents <- report_contents(res)
  expect_named(contents$figures, names(stand_figures(res)))
  for (f in contents$figures) {
    expect_true(nzchar(f$title))
    expect_true(f$height > 0)
    expect_no_error(ggplot2::ggplot_build(
      if (inherits(f$plot, "patchwork")) f$plot[[1]] else f$plot))
  }
  inputs <- contents$inputs
  expect_identical(inputs$species$Label, c("lma = 0.0825", "lma = 0.2"))
  ## lma is the only parameter set here; the trade-offs' derived ones are left
  ## out, though they differ too.
  expect_identical(inputs$parameters$Parameter, "lma")
  ## Values are text, so a small one is not printed as 0.
  expect_identical(inputs$parameters[["Species 2"]], "0.2")
  expect_identical(inputs$parameters$Unit, "kg m\u207b\u00b2")
  expect_identical(inputs$control$Setting, "node_density_coordinate")
  expect_identical(inputs$environment$Variable, "Light")
  expect_null(contents$viewer)
  expect_match(contents$subtitle, "^FF16, 2 species, 40 years$")
})

test_that("a TF24 report adds the water figure and the soil", {
  skip_if_not_installed("plant")
  contents <- report_contents(tf24_run())
  expect_true("water" %in% names(contents$figures))
  ## A patchwork is sized by its panels, two to a row.
  water <- contents$figures$water
  expect_equal(water$height, 2.6 * ceiling(length(water$plot) / 2))
  env <- contents$inputs$environment
  expect_identical(env$Variable, c("Light", "Soil layers", "Mean rainfall"))
  ## One species, but its lma differs from the default strategy's.
  expect_identical(contents$inputs$parameters$Parameter, "lma")
})

test_that("a K93 report shows a parameter one species leaves at its default", {
  skip_if_not_installed("plant")
  contents <- report_contents(k93_run())
  expect_false("water" %in% names(contents$figures))
  pars <- contents$inputs$parameters
  expect_identical(pars$Parameter, "b_0")
  expect_equal(pars$Default, pars[["Species 1"]])
  expect_match(contents$inputs$environment$Value, "stem diameter")
})

test_that("the viewer is embedded on request", {
  skip_if_not_installed("plant")
  page <- report_contents(k93_run(), viewer = TRUE)$viewer
  expect_match(page, "^<!doctype html>")
  expect_match(page, "id=\"stand-data\"", fixed = TRUE)
})

test_that("plant's LaTeX units read as plain text", {
  expect_identical(
    plain_text(c("kg m$^{-2}$", "yr m^${2}$ kg$^{-1}$", NA,
                 "leaf area of 1m$^2$", "kg")),
    c("kg m\u207b\u00b2", "yr m\u00b2 kg\u207b\u00b9", NA,
      "leaf area of 1m\u00b2", "kg"))
})

test_that("numbers in the tables keep small values", {
  expect_identical(number_text(c(3.62847e-15, 0.0825, NA)),
                   c("3.628e-15", "0.0825", ""))
})

test_that("a birth rate that varies is given as its range", {
  expect_identical(birth_rate_text(list(birth_rate_y = 20,
                                        is_variable_birth_rate = FALSE)), "20")
  expect_identical(birth_rate_text(list(birth_rate_y = c(1, 3, 2),
                                        is_variable_birth_rate = TRUE)),
                   "varies, 1 to 3")
})

test_that("environment variables are put back as they were", {
  withr::local_envvar(PLANTPRESS_A = "a", PLANTPRESS_B = NA)
  restore <- set_env(PLANTPRESS_A = "x", PLANTPRESS_B = "y")
  expect_identical(Sys.getenv(c("PLANTPRESS_A", "PLANTPRESS_B"),
                              names = FALSE), c("x", "y"))
  restore()
  expect_identical(Sys.getenv("PLANTPRESS_A"), "a")
  expect_identical(Sys.getenv("PLANTPRESS_B", unset = NA), NA_character_)
})

test_that("a report renders to one self-contained page", {
  skip_if_not_installed("plant")
  skip_on_cran()
  skip_if_not_installed("quarto")
  skip_if(is.null(quarto::quarto_path()), "no Quarto CLI")
  file <- withr::local_tempfile(fileext = ".html")
  expect_identical(stand_report(ff16_run(), file, title = "Two FF16 species",
                                viewer = TRUE), file)
  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  ## grepl(), not expect_match(), which would print the whole page.
  has <- function(text) grepl(text, html, fixed = TRUE)
  expect_true(has("<title>Two FF16 species</title>"))
  for (heading in c("Inputs", "Size distribution through time",
                    "Growth, mortality and fecundity by size", "Stand totals",
                    "Light through the canopy", "The stand in 3D")) {
    expect_true(has(paste0(">", heading, "<")), label = heading)
  }
  ## Figures are embedded, and nothing is fetched except by the viewer, whose
  ## page sits escaped inside the iframe's srcdoc.
  expect_true(has("src=\"data:image/png;base64,"))
  expect_false(grepl("(src|href)=\"https?://", html))
  ## The viewer arrives whole: Quarto once rewrote the table escaped inside
  ## it, cutting it off there.
  srcdoc <- regmatches(html, regexpr("srcdoc=\"[^\"]*\"", html))
  srcdoc <- substr(srcdoc, 9, nchar(srcdoc) - 1)
  ## Quarto writes the attribute back with its own choice of entities.
  codes <- gregexpr("&#[0-9]+;", srcdoc)
  regmatches(srcdoc, codes) <- lapply(regmatches(srcdoc, codes), function(e) {
    vapply(as.integer(gsub("[^0-9]", "", e)), intToUtf8, "")
  })
  entities <- c("&quot;" = "\"", "&lt;" = "<", "&gt;" = ">", "&amp;" = "&")
  for (e in names(entities)) {
    srcdoc <- gsub(e, entities[[e]], srcdoc, fixed = TRUE)
  }
  page <- report_contents(ff16_run(), viewer = TRUE)$viewer
  expect_identical(nchar(srcdoc), nchar(page))
  expect_true(identical(srcdoc, page))
})
