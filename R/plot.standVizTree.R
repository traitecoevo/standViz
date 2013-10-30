#' Plot a single tree in 3D
plot.standVizTree <- function(x, ...) {

    plot3dShape(x$crown, col = x$crownColor)
    plot3dShape(x$stem, col = x$stemColor)
 }
