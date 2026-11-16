#' Write a stand as an interactive 3D page
#'
#' Writes one self-contained HTML file: the three.js stand viewer with the
#' realised stand embedded. It opens straight from disk (three.js loads from a
#' CDN, so it needs a network connection), and can be emailed, embedded in a
#' Quarto page or published.
#'
#' @param stand A `plantpress_stand` from [realise_stand()].
#' @param file Path of the HTML file to write.
#' @param title Page title.
#' @param standalone Wrap the page in its own `<html>` document, so it opens
#'   from disk. `FALSE` writes the bare fragment, for hosts that supply the
#'   document (such as a claude.ai artifact).
#' @param colour_by Further state variables the viewer can colour trees by, as
#'   a named character vector of columns of `stand$rows` (every state variable
#'   from plant and [plant::expand_state()]) with the names as button labels.
#'   Each adds two bytes per tree per frame to the page. The default offers
#'   growth (`net_mass_production_dt`) and leaf area (`area_leaf`) where the
#'   model has them; K93 has neither.
#' @return `file`, invisibly.
#' @export
write_viewer <- function(stand, file, title = "Stand through time",
                         standalone = TRUE, colour_by = NULL) {
  if (is.null(colour_by)) {
    colour_by <- c(Growth = "net_mass_production_dt", `Leaf area` = "area_leaf")
    colour_by <- colour_by[colour_by %in% names(stand$rows)]
  }
  missing <- setdiff(colour_by, names(stand$rows))
  if (length(missing) > 0) {
    stop("Not in the stand: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (is.null(names(colour_by))) names(colour_by) <- colour_by
  names(colour_by)[names(colour_by) == ""] <- colour_by[names(colour_by) == ""]
  template <- system.file("viewer", "viewer.html", package = "plantpress",
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
## range: `size` (the model's size state, in its own units) and `stem_diameter`
## (m) always, and `crown_scale` (m) for a model with crowns
## (`meta$has_crown`), whose species then carry the crown outline the viewer
## turns into a solid of revolution. `colour` lists the extra variables
## offered as colours. The light profile, over the model's size, is row-major,
## one row per frame.
stand_json <- function(stand, colour_by = character(0)) {
  tr <- stand$trees
  r <- stand$rows
  n <- tabulate(r$tree, nrow(tr))
  crowned <- isTRUE(stand$meta$has_crown)
  drawn <- c(size = "height", if (crowned) c(crown_scale = "crown_scale"),
             stem_diameter = "stem_diameter")
  x <- list(
    meta = stand$meta,
    times = stand$times,
    species = lapply(seq_len(nrow(stand$species)), function(k) {
      out <- as.list(stand$species[k, ])
      if (crowned) out$profile <- stand$crowns[[k]]$profile
      out
    }),
    ## I() keeps a stand of one tree as arrays under auto_unbox.
    trees = lapply(list(species = match(tr$species, stand$species$id) - 1L,
                        x = tr$x, y = tr$y, birth = tr$birth_frame - 1L,
                        frames = n, offset = cumsum(n) - n), I),
    rows = lapply(drawn, function(v) pack_u16(r[[v]])),
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
  if (length(v) == 0) return(list(min = 0, scale = 0, data = ""))
  lo <- min(v)
  scale <- (max(v) - lo) / 65535
  q <- if (scale > 0) as.integer(round((v - lo) / scale)) else integer(length(v))
  list(min = lo, scale = scale,
       data = jsonlite::base64_enc(writeBin(q, raw(), size = 2, endian = "little")))
}
