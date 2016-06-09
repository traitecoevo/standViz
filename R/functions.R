#' Creates a 3d shape matrix suitable for passing to \code{rgl} plotting devices,
#' such as \code{triangles3d}. This object has height 1, width 1, and is located at (0,0,0). Use
#' the functions \code{rotate3d, scale3d, translate3d} in \code{rgl} to scale and move to a different set of coordinates
#' @title Creates a 3d shape matrix suitable for plotting with rgl devices
#' @param shape Desired shape of object: "cone", "ellipsoid", "halfellipsoid",
#'  "paraboloid", "cylinder", or "yokozawa".
#' @param nz Number of vertical points used to generate the shape.
#' @param nalpha Number of angles at which nodes in the canopy are created
#' @param zvals Heights to calculate points. Overrides specification of \code{nz}.
#' @param eta Canopy shape factor, required when shape is set as \code{yokozawa}.
#' See \code{yokozawa_density} for details.
#' @return A matrix  with columns [x,y,z] defining the shape. These points define triangles. Note
#' the matrix includes duplicate points because each is used in multiple triangles.
#' @export
tree_shape3d <- function(shape = c("cone", "ellipsoid", "halfellipsoid",
                                  "paraboloid", "cylinder", "yokozawa"),
                      nz = 25, nalpha = 25, zvals = NULL, eta = 4) {

  # Define canopy shape function. Gives relative distance from centre at
  # different relative heights within the crown
  shape <- match.arg(shape)
  if (shape == "cone")
    f <- function(z) (1 - z)
  if (shape == "ellipsoid")
    f <- function(z) sqrt(1 - ( (z - 1 / 2) ^ 2) / ( (1 / 2) ^ 2) )
  if (shape == "halfellipsoid")
    f <- function(z) sqrt(1 - z ^ 2)
  if (shape == "paraboloid")
    f <- function(z) sqrt(1 - z)
  if (shape == "cylinder")
    f <- function(z) 1
  if (shape == "yokozawa") {
    f <- function(z) yokozawa_density(z, 1, eta)
  }
  # Heights to calculate points, if not already defined
  if (is.null(zvals) ) {
    zvals <- seq(1, 0, length = nz)
  } else {
    nz <- length(zvals)
  }
  # Make matrix of x,y,z values, by choosing points with given radius and angle
  # at a given height scaled to specified object width
  radius <- rep(0.5 * f(zvals) / max(f(zvals) ), times = nalpha)
  angles <- rep(seq(0, 2 * pi, length = nalpha), each = nz)
  x <- radius * cos(angles)
  y <- radius * sin(angles)
  z <- rep(zvals, times = nalpha)
  m <- matrix(cbind(x, y, z), ncol = 3)
  # determine ordering of points to make triangles. Each row of tm gives indices
  # for the vertices of a single triangle traverse each vertical spines and
  # define two triangles between each set of four points (adjacent splines and
  # heights)
  tm <- matrix(nrow = 3, ncol = (nalpha - 1) * 2 * (nz - 1) )
  r <- 1
  for (ai in seq_len(nalpha - 1)) {
    # do for all angles do for each height
    for (zi in seq_len(nz - 1)) {
      # first (upper) triangle
      tm[1, r] <- (ai - 1) * nz + zi
      tm[2, r] <- (ai) * nz + zi
      tm[3, r] <- (ai) * nz + zi + 1
      # second (lower) triangle
      tm[1, r + 1] <- (ai - 1) * nz + zi
      tm[2, r + 1] <- (ai - 1) * nz + zi + 1
      tm[3, r + 1] <- (ai) * nz + zi + 1
      # increment index
      r <- r + 2
    }
  }
  ret <- m[tm, 1:3]
}

#' Creates a single tree with specified characteristics. Uses existing shape matrices for \code{stem}
#' and \code{crown} if provided, leading
#' to faster computing times. Otherwise recalculates these.
#' @title Creates a single tree with specified characteristics
#' @param x Location in x dimension
#' @param y Location in x dimension
#' @param height Top height of tree
#' @param height_crown_base Height of crown base
#' @param crown_width Width of crown
#' @param dbh Diameter at breast height
#' @param crown_shape One of 'cone', 'ellipsoid', 'round', 'halfellipsoid', 'paraboloid', 'cylinder', 'yokozawa'
#' @param eta Canopy scaling factor. See \code{yokozawa_density} for details.
#' @param crown_color Colour of the crown
#' @param stem_color Colour of the stem
#' @param nz Number of vertical points used to generate the shape. Only used if zvals=NULL.
#' @param nalpha Number of angles at which nodes in the canopy are created
#' @param crown A previously existing matrix with columns [X,Y,Z] describing shape of the crown
#' @param stem A previously existing matrix with columns [X,Y,Z] describing shape of the stem
#' @param zvals Heights to calculate points. Overrides specification of \code{nz}.
#' @param ... Additional arguments defining crown, to pass through to \code{tree_shape3d}
#' @return A list with elements  "crown" & "stem", ecah of which has elements "shape" and "col"
#' @export
#' @seealso \code{\link{plot.tree3d}}
tree3d <- function(x = 0, y = 0, height = 1, height_crown_base = 0.5,
                      crown_width = 1, dbh = 0.01,
                      crown_shape = c("cone",  "ellipsoid",
                          "round", "halfellipsoid", "paraboloid", "cylinder",
                          "yokozawa"),
                      eta = 13, crown_color = "forestgreen", crown = NULL, stem = NULL,
                      stem_color = "brown",
                      nz = 25, nalpha = 25, zvals = NULL, ...) {

  # Makes a basic crown shape, with height 1.0, width 1.0, located at
  # (0,0,0)
  if (is.null(crown) ) {
    crown <- tree_shape3d(shape = match.arg(crown_shape), eta = eta, nz = nz,
                          nalpha = nalpha, zvals = zvals, ...)
  }

  # Makes a basic stem, with height 1.0, width 1.0, located at (0,0,0)
  if (is.null(stem) ) {
    stem <- tree_shape3d(shape = "cone", nz = 2, nalpha = nalpha, zvals = zvals)
  }

  # Resize based on specified dimensions
  # scale3d scales shape in 3 dimensions by specified amount
  # translate3d shifts shape in 3 dimensions by specified amount

  crown <- rgl::scale3d(crown, x = crown_width, y = crown_width,
                                z = (height - height_crown_base))
  crown <- rgl::translate3d(crown, x = x, y = y, z = height_crown_base)

  stem <- rgl::scale3d(stem, x = dbh, y = dbh, z = height)
  stem <- rgl::translate3d(stem, x = x, y = y, z = 0)

  tree <- list( crown = list(shape = crown, col = crown_color),
                stem =  list(shape = stem, col = stem_color))
  class(tree) <- "tree3d"

  tree
}

#' Takes a dataframe of tree values, checks for essential variables and fills missing
#' values with defaults
#' @title Takes a dataframe of tree values, checks for essential variables and fills missing
#' values with defaults
#' @param trees A dataframe of values for the stand.
#' @param crown_shape Shape of crown. See \code{tree_shape3d} for allowable values.
#' @param eta Canopy shape factor, required when shape is set as \code{yokozawa}.
#' See \code{yokozawa_density} for details.
#' @param crown_color Colour of the crown
#' @param stem_color Colour of the stem
#' @return A dataframe of values for the stand.
#' @export
stand3d_dataframe <- function(trees, crown_shape = "yokozawa", eta = 5,
                                  crown_color = "forestgreen",
                                  stem_color = "brown") {

  if ( is.null(trees) || nrow(trees) == 0 ) {
    return(trees)
  }

  # Fill missing values as needed
  for (v in c("crown_shape", "eta", "crown_color", "stem_color") ) {
    if (is.null(trees[[v]]) )
      trees[[v]] <- get(v)
  }

  trees[["species"]] <- sprintf("%s-%s-%s", trees[["crown_shape"]],
                                      trees[["eta"]], trees[["crown_color"]])

  for (v in c("x", "y", "height", "height_crown_base",
                                            "crown_width", "dbh") ) {
    if (is.null(trees[[v]]) )
      stop(paste(v, "values needed to make stand") )
  }
  trees
}


#' Creates a stand of trees to plot in 3d
#' @title Creates a stand of trees to plot in 3d
#' @param trees A dataframe of values for the stand. Required values are defined in code{stand3d_dataframe}
#' @param ... Additional arguments defining crown, to pass through to \code{tree_shape3d}
#' @return Returns a list of trees in the stand
#' @export
stand3d <- function(trees, ...) {

  trees <- stand3d_dataframe(trees)

  stand <- list()
  t <- 0

  for (s in unique(trees[["species"]]) ) {

    d <- subset(trees, trees[["species"]] == s)

    # Make basic shapes for crown and stem for reuse
    crown <- tree_shape3d(shape = d[["crown_shape"]][1],
                          d[["eta"]][1], ...)

    stem <- tree_shape3d(shape = "cone", nz = 2, nalpha = 5)

    # Add to list
    for (i in seq_len(nrow(d) )) {
      stand[[t + i]] <- tree3d(x = d[["x"]][i],
                                  y = d[["y"]][i],
                                  height = d[["height"]][i],
                                  height_crown_base =
                                                    d[["height_crown_base"]][i],
                                  crown_width = d[["crown_width"]][i],
                                  dbh = d[["dbh"]][i],
                                  crown_color = d[["crown_color"]][i],
                                  stem_color = d[["stem_color"]][i],
                                  crown = crown,
                                  stem = stem)
      }
    t <- t + nrow(d)
  }
  class(stand) <- "trees3d"
  stand
}

#' Creates a new rgl scene with desired dimensions
#' @title Creates a new rgl scene with desired dimensions
#' @param size A vector of dimensions for the plot, as specified in \code{new_rgl}
#' @export
new_rgl <- function(size = c(1, 1, 500, 500) ) {
  rgl::rgl.open()
  rgl::par3d(windowRect = size)
  rgl::par3d(mouseMode = rep("none", 3) )
  rgl::bg3d("white")
  rgl::rgl.viewpoint(0, -65, fov = 0, scale = c(1, 1, 1), zoom = 1)
}


#' Plot a stand of trees. To optimsie computaional performance when plotting via rgl,
#' the function groups together everything with similar colour and plots these in a single call.
#' @title Plot a stand of trees
#' @param stand A list of trees, as generated \code{stand3d}. elements of the list are generated
#' by \code{tree3d}.
#' @param ... Additional arguments passed through to \code{rgl}
#' @export
plot.trees3d <- function(stand, ...) {

  crown_colors <- sapply(stand, function(tree) tree[["crown"]][["col"]])
  stem_colors <- sapply(stand, function(tree) tree[["stem"]][["col"]])

  get_shape <- function(n, stand, v = "crown") stand[[n]][[v]][["shape"]]

  for (i in unique(crown_colors) ) {
    shapes <- do.call(rbind, lapply(which(i == crown_colors), get_shape,
                                        stand = stand, v = "crown") )
    rgl::triangles3d(shapes, col = i, ...)
  }

  for (i in unique(stem_colors) ) {
    shapes <- do.call(rbind, lapply(which(i == stem_colors), get_shape,
                                        stand = stand, v = "stem") )
    rgl::triangles3d(shapes, col = i, ...)
  }
}


#' Plot a single tree in 3d
#' @title Plot a single tree in 3d
#' @param tree Top height of plant
#' @param ... Additional arguments passed through to \code{rgl::rgl.triangles}
#' @export
plot.tree3d <- function(trees, ...) {
  for(v in names(trees)) {
    rgl::triangles3d(tree[[v]][["shape"]], col = tree[[v]][["col"]], ...)
  }
}


#' Function describing vertical distribution of leaf area within the crown of a plant.
#' First described by Yokozawa 1995 doi: 10.1006/anbo.1995.1096
#' @title Function describing vertical distribution of leaf area within the crown
#' @param z Height within canopy of the plant
#' @param h Top height of plant
#' @param eta Canopy shape factor
#' @return A vector of values, with same length as z
#' @export
yokozawa_density <- function(z, h, eta) {
  q <- 2 * eta * (1 - (z / h) ^ eta) * (z / h) ^ eta / z
  q[is.na(q)] <- 0
  q
}
