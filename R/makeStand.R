#' Plot a stand in 3D
#' @description More details on what it does
#' @param treeData blah
#' @param nz Number of 3D triangles in the Z direction (?)
#' @param nalpha Number of angles (...)
#' @param zvals blah
#' @param \dots Further arguments (currently ignored)
#' @export
#' @examples
#' # Example stand plot
#' standViz:::newRgl()
#' noTrees <- 10
#' treeData <- data.frame(topHeight = rep(20, times = noTrees),
#' heightCrownBase = rep(0,times = noTrees),
#' crownWidth = rep(10, times = noTrees),
#' dbh = rep(0.5, times = noTrees),
#' crownShape = "yokozawa", eta = 10,
#' x = runif(noTrees, min = 1, max = 10),
#' y = runif(noTrees, min = 1, max = 10))
#' myStand <- makeStand(treeData)
#' plot(myStand)

makeStand <- function(treeData, nz = 25, nalpha = 25, zvals = NA, ...) {

    myTreeData <- checkStandDataForMissingValues(treeData)

    stand <-list()
    t=0

    for(s in unique(myTreeData$species)){

        treeData.sp <- subset(myTreeData, myTreeData$species ==s)

        # Make basic shapes for crown and stem for reuse
        crownShapeMatrix <- make3dShape(shape = as.character(treeData.sp$crownShape[1]),
            treeData.sp$eta[1], nz = nz, nalpha = nalpha, zvals = zvals)

        stemShapeMatrix <- make3dShape(shape = "cone", nz = 2, nalpha = 5)

        noTrees <- nrow(treeData.sp)

        if(!is.null(noTrees)){
            for (i in 1:noTrees)
                stand[[t+i]] <- makeTree(
                    x=treeData.sp$x[i],
                    y=treeData.sp$y[i],
                    topHeight=treeData.sp$topHeight[i],
                    heightCrownBase=treeData.sp$heightCrownBase[i],
                    crownWidth=treeData.sp$crownWidth[i],
                    dbh=treeData.sp$dbh[i],
                    crownShapeMatrix=crownShapeMatrix,
                    stemShapeMatrix=stemShapeMatrix,
                    crownColor=treeData.sp$crownColor[i],
                    stemColor=treeData.sp$stemColor[i])
            t= t+i
        }

    }
    class(stand) <- "standVizStand"
    stand
}
