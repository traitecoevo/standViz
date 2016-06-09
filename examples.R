library(treesrgl)

#' # Plot the crown profile function for Yokozawa's function:
#' z <- seq(0, 1, 0.01)
#' q <- yokozawa_density(z, 1, 12)
#' plot(q, z, type = 'l')
#'
new_rgl()


zvals <- c(0, seq(0.4, 0.9, by = 0.025), seq(0.91, 1, by = 0.01))

x <- make_tree(crown_shape = "yokozawa", crown_width = 5, top_height = 10, height_crown_base = 0,
  dbh = 0.1, eta = 13, zvals = zvals)

make_tree <- function(x = 0, y = 0, top_height = 1, height_crown_base = 0, crown_width = 1,
  dbh = 0.01, crown = NA, crown_shape = c("cone", "elipsoid", "ellipsoid",
    "round", "halfellipsoid", "paraboloid", "cylinder", "yokozawa"), eta = 13,
  crown_color = "forestgreen", stem = NA, stem_color = "brown", nz = 25,
  nalpha = 25, zvals = NA, scaling = c(1, 1), ...)