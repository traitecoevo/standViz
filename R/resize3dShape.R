# Scale and relocate shape.
# Assumes shape is initially located at (0,0,0)
# The shape is defined by a shapeMatrix with columns [X,Y,Z]

resize3dShape <- function(shapeMatrix, height = 1, width = 1, x0 = 0, y0 = 0, z0 = 0) {
    shapeMatrix[, 1] <- x0 + 0.5 * width * (shapeMatrix[, 1]/max(shapeMatrix[, 1]))
    shapeMatrix[, 2] <- y0 + 0.5 * width * (shapeMatrix[, 2]/max(shapeMatrix[, 2]))
    shapeMatrix[, 3] <- z0 + height * (shapeMatrix[, 3]/max(shapeMatrix[, 3]))
    return(shapeMatrix)
}
