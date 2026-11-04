#' One row per living tree per frame
#'
#' Expands a realised stand into a long table, for renderers and analyses that
#' want every tree's size at every frame (a Blender script, a check against
#' plant's size distribution).
#'
#' @param stand A `standviz_stand` from [realise_stand()].
#' @return A data frame with `frame`, `time`, `tree`, `species`, `x`, `y`,
#'   `height`, `crown_scale`, `crown_radius` (widest point), `stem_diameter`
#'   (m) and `light_crown`: the canopy openness at the crown's leaf-weighted
#'   centre.
#' @export
stand_long <- function(stand) {
  tr <- stand$trees
  n_frames <- length(stand$times)
  len <- pmin(tr$death_frame - 1, n_frames) - tr$birth_frame + 1
  tree <- rep(seq_len(nrow(tr)), len)
  frame <- sequence(len, from = tr$birth_frame)
  k <- match(tr$species[tree], stand$species$id)
  i <- tr$interval[tree]
  v <- tr$v[tree]

  ## Each tree sits a fixed fraction v of the way between its two nodes.
  at_tree <- function(what) {
    out <- numeric(length(tree))
    for (s in unique(k)) {
      m <- stand$nodes[[s]][[what]]
      j <- which(k == s)
      out[j] <- m[cbind(frame[j], i[j])] * (1 - v[j]) +
        m[cbind(frame[j], i[j] + 1)] * v[j]
    }
    out
  }
  height <- at_tree("height")
  crown_scale <- at_tree("crown_scale")
  widest <- vapply(stand$species$eta, function(eta) {
    max(crown_profile(seq(0, 1, by = 0.001), eta))
  }, 0)

  data.frame(frame = frame, time = stand$times[frame], tree = tree,
             species = tr$species[tree], x = tr$x[tree], y = tr$y[tree],
             height = height, crown_scale = crown_scale,
             crown_radius = crown_scale * widest[k],
             stem_diameter = at_tree("stem_diameter"),
             light_crown = light_at(stand$light, frame,
                                    height * stand$species$eta_c[k]))
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
#' @return `file`, invisibly.
#' @export
write_viewer <- function(stand, file, title = "Stand through time",
                         standalone = TRUE) {
  template <- system.file("viewer", "viewer.html", package = "standviz",
                          mustWork = TRUE)
  html <- paste(readLines(template, warn = FALSE), collapse = "\n")
  ## A "</" inside the data would close its <script> element.
  json <- gsub("</", "<\\/", stand_json(stand), fixed = TRUE)
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

## The viewer's contract. Frames are 0-based in JSON; matrices are row-major,
## one row per frame; NA (a node not yet born) becomes null.
stand_json <- function(stand) {
  tr <- stand$trees
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
    nodes = lapply(stand$nodes, function(n) {
      list(height = n$height, crown_scale = n$crown_scale,
           stem_diameter = n$stem_diameter)
    }),
    trees = list(species = match(tr$species, stand$species$id) - 1L,
                 interval = tr$interval - 1L, v = tr$v, x = tr$x, y = tr$y,
                 birth = tr$birth_frame - 1L, death = tr$death_frame - 1L),
    light = stand$light)
  jsonlite::toJSON(x, digits = I(4), na = "null", auto_unbox = TRUE,
                   matrix = "rowmajor")
}
