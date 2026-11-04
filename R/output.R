#' One row per living tree per frame
#'
#' Expands a realised stand into a long table, for renderers and analyses that
#' want every tree's size at every frame (a Blender script, a check against
#' plant's size distribution).
#'
#' @param stand A `standviz_stand` from [realise_stand()].
#' @return A data frame with `frame`, `time`, `tree`, `species`, `x`, `y`,
#'   `height`, `crown_scale`, `crown_radius` (widest point), `stem_diameter`
#'   (m), `light_crown` (the canopy openness at the crown's leaf-weighted
#'   centre), and every other state variable the stand carries, such as
#'   `area_leaf`, `mass_live`, `fecundity` and `net_mass_production_dt`.
#' @export
stand_long <- function(stand) {
  r <- stand$rows
  tr <- stand$trees
  k <- match(tr$species[r$tree], stand$species$id)
  out <- data.frame(frame = r$frame, time = stand$times[r$frame], tree = r$tree,
                    species = tr$species[r$tree], x = tr$x[r$tree], y = tr$y[r$tree],
                    height = r$height, crown_scale = r$crown_scale,
                    crown_radius = r$crown_scale * crown_widest(stand$species$eta)[k],
                    stem_diameter = r$stem_diameter,
                    light_crown = light_at(stand$light, r$frame,
                                           r$height * stand$species$eta_c[k]))
  cbind(out, r[setdiff(names(r), names(out))])
}

## Canopy openness at height z in frame f, linear on the light grid.
light_at <- function(light, frame, z) {
  g <- light$height
  pos <- pmin(pmax((z - g[1]) / (g[2] - g[1]), 0), length(g) - 1 - 1e-9)
  lo <- floor(pos) + 1
  w <- pos - (lo - 1)
  light$value[cbind(frame, lo)] * (1 - w) + light$value[cbind(frame, lo + 1)] * w
}

#' Write a stand as an interactive 3D page
#'
#' Writes one self-contained HTML file: the three.js stand viewer with the
#' realised stand embedded. It opens straight from disk (three.js loads from a
#' CDN, so it needs a network connection), and can be emailed, embedded in a
#' Quarto page or published.
#'
#' @param stand A `standviz_stand` from [realise_stand()].
#' @param file Path of the HTML file to write.
#' @param title Page title.
#' @param standalone Wrap the page in its own `<html>` document, so it opens
#'   from disk. `FALSE` writes the bare fragment, for hosts that supply the
#'   document (such as a claude.ai artifact).
#' @param colour_by Further state variables the viewer can colour trees by, as
#'   a named character vector of columns of `stand$rows` (every state variable
#'   from plant and [plant::expand_state()]) with the names as button labels.
#'   Each adds two bytes per tree per frame to the page.
#' @return `file`, invisibly.
#' @export
write_viewer <- function(stand, file, title = "Stand through time",
                         standalone = TRUE,
                         colour_by = c(Growth = "net_mass_production_dt",
                                       `Leaf area` = "area_leaf")) {
  missing <- setdiff(colour_by, names(stand$rows))
  if (length(missing) > 0) {
    stop("Not in the stand: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (is.null(names(colour_by))) names(colour_by) <- colour_by
  names(colour_by)[names(colour_by) == ""] <- colour_by[names(colour_by) == ""]
  template <- system.file("viewer", "viewer.html", package = "standviz",
                          mustWork = TRUE)
  html <- paste(readLines(template, warn = FALSE), collapse = "\n")
  ## A "</" inside the data would close its <script> element.
  json <- gsub("</", "<\\/", stand_json(stand, colour_by), fixed = TRUE)
  html <- splice(html, "__STAND_TITLE__", title)
  html <- splice(html, "__STAND_DATA__", json)
  if (standalone) {
    html <- paste0("<!doctype html>\n<html lang=\"en\">\n<head>\n",
                   "<meta charset=\"utf-8\">\n",
                   "<meta name=\"viewport\" content=\"width=device-width, ",
                   "initial-scale=1, viewport-fit=cover\">\n</head>\n<body>\n",
                   html, "\n</body>\n</html>")
  }
  writeLines(html, file)
  invisible(file)
}

## Replace every occurrence of token, taking value literally (sub() would read
## backslashes in it as escapes).
splice <- function(text, token, value) {
  parts <- strsplit(paste0(text, "\x01"), token, fixed = TRUE)[[1]]
  out <- paste(parts, collapse = value)
  substr(out, 1, nchar(out) - 1)
}

## The viewer's contract. Frames are 0-based. Each tree's visible frames are
## one unbroken run, stored contiguously in the row arrays from `offset`, in
## frame order. Per-row values are 16-bit integers spanning the variable's
## range; `colour` lists the extra variables offered as colours. The light
## profile is row-major, one row per frame.
stand_json <- function(stand, colour_by = character(0)) {
  tr <- stand$trees
  r <- stand$rows
  n <- tabulate(r$tree, nrow(tr))
  x <- list(
    meta = stand$meta,
    times = stand$times,
    species = lapply(seq_len(nrow(stand$species)), function(k) {
      ## The crown outline the viewer turns into a solid of revolution, drawn
      ## down to the height that holds 95% of the leaf area: below it the
      ## profile is a long, near-empty tail that reads as a funnel.
      eta <- stand$species$eta[k]
      u_base <- (1 - sqrt(0.95))^(1 / eta)
      u <- u_base + (1 - u_base) * (1 - cos(seq(0, pi, length.out = 33))) / 2
      c(as.list(stand$species[k, ]),
        list(profile = list(u = c(u_base, u), r = c(0, crown_profile(u, eta)))))
    }),
    trees = list(species = match(tr$species, stand$species$id) - 1L,
                 x = tr$x, y = tr$y, birth = tr$birth_frame - 1L, frames = n,
                 offset = c(0L, cumsum(n)[-length(n)])),
    rows = lapply(c(height = "height", crown_scale = "crown_scale",
                    stem_diameter = "stem_diameter"),
                  function(v) pack_u16(r[[v]])),
    colour = lapply(seq_along(colour_by), function(i) {
      c(list(label = names(colour_by)[i], column = unname(colour_by[i])),
        pack_u16(r[[colour_by[i]]]))
    }),
    light = stand$light)
  jsonlite::toJSON(x, digits = I(4), na = "null", auto_unbox = TRUE,
                   matrix = "rowmajor")
}

## Values as 16-bit integers spanning their range (min + q * scale), base64.
## Net production, for one, is negative in deep shade.
pack_u16 <- function(v) {
  lo <- min(v)
  scale <- (max(v) - lo) / 65535
  q <- if (scale > 0) as.integer(round((v - lo) / scale)) else integer(length(v))
  list(min = lo, scale = scale,
       data = jsonlite::base64_enc(writeBin(q, raw(), size = 2, endian = "little")))
}
