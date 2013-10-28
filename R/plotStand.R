#' Plot a stand in 3D
#' @description More details on what it does
#' @param siteData blah
#' @param treeData blah
#' @param verbose Logical
#' @param nz Number of 3D triangles in the Z direction (?)
#' @param nalpha Number of angles (...)
#' @param zvals blah
#' @param \dots Further arguments (currently ignored)
#' @export
#' @examples
#' # Example stand plot
#' siteData <- list(x0=0,
#' xmax=10,
#' y0=0,
#' ymax=10)
#' siteData$area <- with(siteData, (xmax - x0) * (ymax - y0))
#' 
#' standViz:::newRgl()
#' noTrees <- 10
#' treeData <- data.frame(topHeight = rep(20, times = noTrees), 
#' heightCrownBase = rep(0,times = noTrees), 
#' crownWidth = rep(10, times = noTrees), 
#' dbh = rep(0.5, times = noTrees),
#' crownShape = "yokozawa", eta = 10)
#' plotStand(siteData, treeData, verbose = FALSE)
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

        species <- paste(treeData$crownShape, treeData$eta, treeData$crownColor)
        stemShapeMatrix <- make3dShape(shape = "cone", nz = 2, nalpha = 5)

        if (verbose)
            message("\nPlotting ", noTrees, " trees from ", length(unique(species)), " species\n")

        for(s in unique(species)){

            treeData.sp <- subset(treeData, s==species)

            if (verbose)
                message("\nPlotting ", nrow(treeData.sp), " trees from species ", s, "\n")

            # Make basic shapes for crown and stem - these are reused, saves recomputing them
            crownShapeMatrix <- make3dShape(shape = as.character(treeData.sp$crownShape[1]),
            treeData.sp$eta[1], nz = nz, nalpha = nalpha, zvals = zvals)

            crowns <- stems <- list()
            for (i in 1:nrow(treeData.sp)) {
                crowns[[i]] <- getCrownTriangles(X = treeData.sp$X[i], Y = treeData.sp$Y[i], topHeight = treeData.sp$topHeight[i], heightCrownBase = treeData.sp$heightCrownBase[i], crownWidth = treeData.sp$crownWidth[i], crownShapeMatrix = crownShapeMatrix)
                stems[[i]] <- getStemTriangles(X = treeData.sp$X[i], Y = treeData.sp$Y[i], topHeight = treeData.sp$topHeight[i], dbh = treeData.sp$dbh[i], stemShapeMatrix = stemShapeMatrix)
            }

            plot3dShape(do.call(rbind,crowns) , col = treeData.sp$crownColor[1])
            plot3dShape(do.call(rbind,stems), col = treeData.sp$stemColor[1])
        }
    }
}
