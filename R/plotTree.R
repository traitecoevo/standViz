# plots a tree if available, uses stem and cronw shape Matrices provided, leading
# to faster computing times. Otherwise recalculates these
plotTree <- function(X = 0, Y = 0, topHeight = 1, heightCrownBase = 0, crownWidth = 1,
    dbh = 0.01, crownShapeMatrix = NA, stemShapeMatrix = NA, crownShape = c("cone",
        "elipsoid", "ellipsoid", "round", "halfellipsoid", "paraboloid", "cylinder",
        "yokozawa"), eta = 13, crownColor = "forestgreen", stemColor = "brown", nz = 25,
    nalpha = 25, zvals = NA, ...) {
    # Makes a basic crown shape, with topHeight 1.0, width 1.0, located at (0,0,0)
    if (is.na(crownShapeMatrix[1])) {
        crownShapeMatrix <- make3dShape(shape = match.arg(crownShape), eta = eta,
            nz = nz, nalpha = nalpha, zvals = zvals)
    }
    # Makes a basic stemShape, with topHeight 1.0, width 1.0, located at (0,0,0)
    if (is.na(stemShapeMatrix[1])) {
        stemShapeMatrix <- make3dShape(shape = "cone", nz = 2, nalpha = nalpha, zvals = zvals)
    }
    # resize and relocate tree crown, then pass to plot
    plot3dShape(resize3dShape(crownShapeMatrix, height = (topHeight - heightCrownBase),
        width = crownWidth, x0 = X, y0 = Y, z0 = heightCrownBase), col = crownColor)
    # resize and relocate tree trunk, then pass to plot
    plot3dShape(resize3dShape(stemShapeMatrix, height = topHeight, width = dbh, x0 = X,
        y0 = Y, z0 = 0), col = stemColor)
}
