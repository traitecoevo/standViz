plotStand <- function(siteData, treeData, verbose = FALSE, nz = 25, nalpha = 25,
    zvals = NA, ...) {
    bg3d("white")
    # make new Stand
    M <- matrix(c(siteData$x0, siteData$y0, 0, siteData$xmax, siteData$y0, 0, siteData$xmax,
        siteData$ymax, 0, siteData$x0, siteData$ymax, 0, siteData$x0, siteData$y0,
        0), ncol = 3, byrow = TRUE)
    lines3d(M, col = "darkgrey")
    noTrees <- length(treeData[, 1])
    if (noTrees > 0) {
        # Check for missing data
        if (is.null(treeData$crownShape))
            treeData$crownShape <- rep("yokozawa", noTrees)
        if (is.null(treeData$eta))
            treeData$crownShape <- rep(5, noTrees)
        if (is.null(treeData$crownColor))
            treeData$crownColor <- rep("forestgreen", noTrees)
        if (is.null(treeData$stemColor))
            treeData$stemColor <- rep("brown", noTrees)
        if (is.null(treeData$X))
            treeData$X <- runif(noTrees, min = siteData$x0, max = siteData$xmax)
        if (is.null(treeData$Y))
            treeData$Y <- runif(noTrees, min = siteData$y0, max = siteData$ymax)
        # Make basic shapes for crown and stem - these are reused, saves recomputing them
        # TO DO: possibility of different shapes in same stand,
        crownShapeMatrix <- make3dShape(shape = as.character(treeData$crownShape[1]),
            treeData$eta[1], nz = nz, nalpha = nalpha, zvals = zvals)
        stemShapeMatrix <- make3dShape(shape = "cone", nz = 2, nalpha = 5)
        if (verbose) {
            message("\nPlotting ", noTrees, " trees\n")
        }
        for (i in 1:noTrees) {
            if (verbose) {
                cat(i, " ")
            }
            plotTree(X = treeData$X[i], Y = treeData$Y[i], topHeight = treeData$topHeight[i],
                heightCrownBase = treeData$heightCrownBase[i], crownWidth = treeData$crownWidth[i],
                dbh = treeData$dbh[i], crownShapeMatrix = crownShapeMatrix, stemShapeMatrix = stemShapeMatrix,
                crownColor = treeData$crownColor[i], stemColor = "brown")
        }
    }
}
