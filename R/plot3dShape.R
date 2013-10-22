plot3dShape <- function(shapeMatrix, ...) {
    rgl.triangles(shapeMatrix[, 1], shapeMatrix[, 2], shapeMatrix[, 3], ...)
}
