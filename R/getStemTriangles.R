#' Plot a single tree in 3D
#' @description Plots a tree if available, uses stem and cronw shape Matrices provided, leading 
#' to faster computing times. Otherwise recalculates these.
#' @param X
#' @param Y
#' @param topHeight
#' @param heightCrownBase
#' @param crownWidth
#' @param dbh
#' @param crownShapeMatrix
#' @param stemShapeMatrix
#' @param crownShape One of "cone", "elipsoid", "ellipsoid", "round", "halfellipsoid", "paraboloid", "cylinder", "yokozawa"
#' @param eta
#' @param crownColor
#' @param stemColor
#' @param nz
#' @param nalpha
#' @param zvals
#' @export
#' @seealso \code{\link{plotStand}}
#' @examples
#' # some examples here.
#' standViz:::newRgl()
#' zvals <- c(0, seq(0.4, 0.9, by = 0.025), seq(0.91, 1, by = 0.01))
#' plotTree(crownShape = "yokozawa", crownWidth = 5, topHeight = 10, heightCrownBase = 0,
#' dbh = 0.1, eta = 13, zvals = zvals)
getStemTriangles <- function(X = 0, Y = 0, topHeight = 1,
    dbh = 0.01, stemShapeMatrix = NA, nz = 25,
    nalpha = 25, zvals = NA, ...) {

    # Makes a basic stemShape, with topHeight 1.0, width 1.0, located at (0,0,0)
    if (is.na(stemShapeMatrix[1]))
        stemShapeMatrix <- make3dShape(shape = "cone", nz = 2, nalpha = nalpha, zvals = zvals)

    # resize and relocate tree trunk
    resize3dShape(stemShapeMatrix, height = topHeight, width = dbh, x0 = X, y0 = Y, z0 = 0)
}
