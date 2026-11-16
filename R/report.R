#' A report on a run
#'
#' Renders one HTML page: the run's inputs (the parameters that differ
#' between its species or from the model's defaults, their birth rates, the
#' numerical settings that differ from plant's, and the environment), then
#' every figure [stand_figures()] draws for it, each with a short account of
#' what it shows, and optionally the 3D viewer. It works for any model,
#' because the figures attach by what the run carries. The page holds its
#' figures and needs nothing else, except the embedded viewer, which loads
#' three.js from a CDN.
#'
#' The report is a Quarto document (`inst/report/stand-report.qmd`). Its
#' contents are computed here, in this R session, and the document only lays
#' them out. It needs the Quarto CLI and the `quarto`, `knitr` and
#' `rmarkdown` packages. Quarto runs the document in a new R process, which
#' is given this session's libraries: reading the saved figures loads plant
#' and plantpress, whose objects the figures keep.
#'
#' @inheritParams plot_by_size
#' @param file Path of the HTML file to write.
#' @param title The report's title.
#' @param viewer Embed the 3D viewer of the stand ([realise_stand()] and
#'   [write_viewer()] with their defaults). It needs a run carried in birth
#'   date, and adds several MB to the page for a long run.
#' @param quiet Suppress Quarto's progress output.
#' @return `file`, invisibly.
#' @export
#' @examples
#' \dontrun{
#' res <- plant::run(p, control = plant::control(
#'   node_density_coordinate = "birth_date"), record = "steps")
#' stand_report(res, "stand-report.html", viewer = TRUE)
#' }
stand_report <- function(x, file, ages = NULL, title = "Stand report",
                         viewer = FALSE, quiet = TRUE) {
  if (!requireNamespace("quarto", quietly = TRUE) ||
      is.null(quarto::quarto_path())) {
    stop("stand_report() needs the Quarto CLI and the quarto package; ",
         "stand_figures() gives the same figures without them", call. = FALSE)
  }
  contents <- report_contents(x, ages = ages, title = title, viewer = viewer)
  dir <- tempfile("plantpress-report-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  input <- file.path(dir, "stand-report.qmd")
  file.copy(system.file("report", "stand-report.qmd", package = "plantpress",
                        mustWork = TRUE), input)
  saveRDS(contents, file.path(dir, "contents.rds"))
  ## Quarto runs the document in a new R process: give it this session's R
  ## and libraries, so it finds the packages that drew the figures.
  restore <- set_env(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep),
                     QUARTO_R = R.home("bin"))
  on.exit(restore(), add = TRUE)
  quarto::quarto_render(input, execute_params = list(contents = "contents.rds"),
                        metadata = list(title = contents$title,
                                        subtitle = contents$subtitle),
                        quiet = quiet, as_job = FALSE)
  if (!file.copy(file.path(dir, "stand-report.html"), file, overwrite = TRUE)) {
    stop("Could not write ", file, call. = FALSE)
  }
  invisible(file)
}

## Everything the report shows, computed from the run: its title, the input
## tables, each figure with its heading, account and height (inches), and the
## viewer page or NULL. The Quarto document only lays these out.
report_contents <- function(x, ages = NULL, title = "Stand report",
                            viewer = FALSE) {
  res <- tidied_run(x)
  figures <- stand_figures(res, ages)
  sections <- lapply(names(figures), function(key) {
    about <- report_sections[[key]]
    if (is.null(about)) about <- list(title = key, caption = "", height = 5)
    plot <- figures[[key]]
    if (inherits(plot, "patchwork")) {
      about$height <- 2.6 * ceiling(length(plot) / 2)
    }
    c(about, list(plot = plot))
  })
  names(sections) <- names(figures)
  list(title = title,
       subtitle = sprintf("%s, %d species, %s years",
                          plant::model_size(res)$model,
                          length(res$p$strategies),
                          format(res$p$max_patch_lifetime)),
       inputs = report_inputs(res),
       figures = sections,
       viewer = if (viewer) viewer_page(res))
}

## Each figure's heading and an account of what it shows, by its name in
## stand_figures(). A figure with no entry is shown under its name; heights
## of the patchworks follow their panel count.
report_sections <- list(
  size_distribution = list(
    title = "Size distribution through time",
    caption = paste(
      "Each line is a node: the plants born at one time, followed as they",
      "grow. Its shade is the node's density relative to the densest, so",
      "nodes thinned by mortality fade."),
    height = 4.5),
  by_size = list(
    title = "Growth, mortality and fecundity by size",
    caption = paste(
      "Each node's rates against its size at chosen patch ages, over the",
      "collected step ending at each age."),
    height = 7),
  stand_totals = list(
    title = "Stand totals",
    caption = paste(
      "Totals per unit ground area, integrated over the size distribution.",
      "Biomass is summed over species and split by tissue.")),
  light_profile = list(
    title = "Light through the canopy",
    caption = paste(
      "The fraction of full light reaching each size, at chosen patch ages."),
    height = 4.5),
  water = list(
    title = "Soil water and plant water use",
    caption = paste(
      "Rainfall and transpiration are rates over each collected step; soil",
      "water is drawn layer by layer, and each node's water potentials,",
      "conductance, profit and leaf temperature along its life.")))

## The run's inputs as tables, each a data frame with readable column names.
## A table that would be empty is NULL.
report_inputs <- function(res) {
  size <- plant::model_size(res)
  run <- data.frame(
    Setting = c("Model", "Species", "Patch lifetime (years)", "Size",
                "Collected steps", "plant", "plantpress"),
    Value = c(size$model, length(res$p$strategies),
              format(res$p$max_patch_lifetime),
              sprintf("%s (%s)", size$size, size$size_units),
              nrow(res$steps),
              as.character(utils::packageVersion("plant")),
              as.character(utils::packageVersion("plantpress"))))
  species <- data.frame(
    Species = seq_along(res$p$strategies),
    Label = species_labels(res$p),
    birth_rate = vapply(res$p$strategies, birth_rate_text, ""),
    `Offspring production` = number_text(res$offspring_production),
    check.names = FALSE)
  names(species)[3] <- "Birth rate (m\u207b\u00b2 yr\u207b\u00b9)"
  list(run = run, species = species,
       parameters = differing_parameters(res$p),
       control = differing_control(res$control),
       environment = environment_table(res))
}

## Numbers as text to four significant digits: a table would round a value
## of 1e-15 to 0. NA is blank.
number_text <- function(v) {
  vapply(v, function(x) if (is.na(x)) "" else format(signif(x, 4)), "")
}

## A strategy's birth rate: its constant value, or the range of a rate that
## varies through time.
birth_rate_text <- function(s) {
  y <- s$birth_rate_y
  if (isTRUE(s$is_variable_birth_rate) && length(unique(y)) > 1) {
    sprintf("varies, %s to %s", format(signif(min(y), 3)),
            format(signif(max(y), 3)))
  } else {
    format(signif(y[[1]], 3))
  }
}

## The parameters a user sets (plant's parameter table's traits, parameters
## and strategy fields, not the derived ones) whose value differs between
## species or from the model's default strategy, with plant's description and
## unit. The default is the model's, not the run's own strategy_default: a
## value set there before adding strategies is shared by every species, and
## is still an input of the run.
differing_parameters <- function(p) {
  table <- plant::parameter_table(p)
  table <- table[table$group %in% c("trait", "parameter", "strategy"), ]
  value <- function(s, name) {
    v <- if (name %in% names(s$pars)) s$pars[[name]] else s[[name]]
    if (is.numeric(v) && length(v) == 1) v else NA_real_
  }
  model <- plant::model_size(p)$model
  default <- vapply(table$name, value, 0, s = plant::default_parameters(
    model, quiet = TRUE)$strategy_default)
  vals <- vapply(p$strategies, function(s) vapply(table$name, value, 0, s = s),
                 numeric(nrow(table)))
  vals <- matrix(vals, nrow = nrow(table))
  same <- function(a, b) {
    (is.na(a) & is.na(b)) |
      (!is.na(a) & !is.na(b) & abs(a - b) <= 1e-8 * pmax(abs(a), abs(b)))
  }
  differs <- !apply(vals, 2, same, b = default)
  differs <- matrix(differs, nrow = nrow(table))
  keep <- rowSums(differs) > 0
  if (!any(keep)) return(NULL)
  blank <- function(x) ifelse(is.na(x), "", x)
  out <- data.frame(Parameter = table$name[keep],
                    Description = blank(plain_text(table$description[keep])),
                    Unit = blank(plain_text(table$unit[keep])),
                    Default = number_text(unname(default[keep])))
  for (k in seq_len(ncol(vals))) {
    out[[paste("Species", k)]] <- number_text(vals[keep, k])
  }
  rownames(out) <- NULL
  out
}

## plant's parameter documentation is written in LaTeX ("kg m$^{-2}$"); the
## report has no maths renderer, so exponents become superscript characters
## and the remaining markup is dropped.
plain_text <- function(x) {
  superscript <- function(s) {
    chartr("-0123456789", paste0("\u207b\u2070\u00b9\u00b2\u00b3\u2074",
                                 "\u2075\u2076\u2077\u2078\u2079"),
           gsub("[^-0-9]", "", s))
  }
  ## "$^{-2}$" and "$^2$", and plant's one "^${2}$".
  exponent <- "\\$\\^\\{?-?[0-9]+\\}?\\$|\\^\\$\\{-?[0-9]+\\}\\$"
  ok <- !is.na(x)
  at <- gregexpr(exponent, x[ok])
  regmatches(x[ok], at) <- lapply(regmatches(x[ok], at), superscript)
  x <- gsub("\\\\(textrm|rm)\\s*", "", x)
  gsub("[$}{]", "", x)
}

## The run's numerical settings that differ from plant's defaults.
differing_control <- function(ctrl) {
  if (is.null(ctrl)) return(NULL)
  default <- plant::control()
  text <- function(v) paste(format(v, digits = 15), collapse = ", ")
  names <- names(unclass(ctrl))
  run <- vapply(names, function(n) text(ctrl[[n]]), "")
  def <- vapply(names, function(n) text(default[[n]]), "")
  ## By value: printed to 7 digits, a finer change would not show.
  keep <- !vapply(names, function(n) {
    isTRUE(all.equal(ctrl[[n]], default[[n]], tolerance = 0,
                     check.attributes = FALSE))
  }, TRUE)
  if (!any(keep)) return(NULL)
  data.frame(Setting = names[keep], Value = run[keep], Default = def[keep],
             row.names = NULL)
}

## What the run's environment records, one row each, by what the run carries.
environment_table <- function(res) {
  env <- res$env
  rows <- list()
  light <- env$light_availability
  if (!is.null(light$light_availability)) {
    rows[["Light"]] <- sprintf("recorded by %s at %d steps",
                               plant::model_size(res)$size,
                               length(unique(light$time)))
  }
  if (!is.null(env$soil_depth)) {
    first <- env$soil_depth[env$soil_depth$step == min(env$soil_depth$step), ]
    rows[["Soil layers"]] <- sprintf("%d, to %s m", nrow(first),
                                     format(signif(max(first$soil_depth), 3)))
  }
  flux <- env$soil_moist_cumulative_flux
  if (!is.null(flux$sum_rainfall) && nrow(flux) > 1) {
    span <- diff(range(flux$time))
    total <- flux$sum_rainfall[which.max(flux$time)] -
      flux$sum_rainfall[which.min(flux$time)]
    rows[["Mean rainfall"]] <- sprintf("%s m yr\u207b\u00b9",
                                       format(signif(total / span, 3)))
  }
  if (length(rows) == 0) return(NULL)
  data.frame(Variable = names(rows), Value = unlist(rows), row.names = NULL)
}

## The 3D viewer of the run's stand as a complete page, for the report to
## embed in an iframe of its own, so its styles stay apart from the report's.
viewer_page <- function(res) {
  stand <- realise_stand(res)
  file <- tempfile(fileext = ".html")
  on.exit(unlink(file))
  write_viewer(stand, file, title = "The stand through time")
  paste(readLines(file, warn = FALSE), collapse = "\n")
}

## Set environment variables; the function returned puts them back.
set_env <- function(...) {
  new <- c(...)
  old <- Sys.getenv(names(new), unset = NA, names = TRUE)
  do.call(Sys.setenv, as.list(new))
  function() {
    unset <- is.na(old)
    if (any(unset)) Sys.unsetenv(names(old)[unset])
    if (any(!unset)) do.call(Sys.setenv, as.list(old[!unset]))
  }
}
