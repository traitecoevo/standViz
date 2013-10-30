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
makeTree <- function(X = 0, Y = 0,
    topHeight = 1, heightCrownBase = 0, crownWidth = 1, dbh = 0.01,
    crownShapeMatrix = NA, crownShape = c("cone",
        "elipsoid", "ellipsoid", "round", "halfellipsoid", "paraboloid", "cylinder",
        "yokozawa"), eta = 13, crownColor = "forestgreen",
    stemShapeMatrix = NA, stemColor = "brown",
    nz = 25, nalpha = 25, zvals = NA,
    ...) {

    # Makes a basic crown shape, with topHeight 1.0, width 1.0, located at (0,0,0)
    if (is.na(crownShapeMatrix[1]))
        crownShapeMatrix <- make3dShape(shape = match.arg(crownShape), eta = eta, nz = nz, nalpha = nalpha, zvals = zvals)

    # Makes a basic stemShape, with topHeight 1.0, width 1.0, located at (0,0,0)
    if (is.na(stemShapeMatrix[1]))
        stemShapeMatrix <- make3dShape(shape = "cone", nz = 2, nalpha = nalpha, zvals = zvals)

    myTree <- list(
        crown = resize3dShape(crownShapeMatrix, height = topHeight - heightCrownBase, width = crownWidth, x0 = X, y0 = Y, z0 = heightCrownBase),
        crownColor = crownColor,
        stem = resize3dShape(stemShapeMatrix, height = topHeight, width = dbh, x0 = X, y0 = Y, z0 = 0),
        stemColor = stemColor)

    class(myTree) <- "standVizTree"

    myTree
}
