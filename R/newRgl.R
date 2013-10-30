# Example plot - tree
newRgl <- function(size=c(1, 1, 500, 500), mouseMode = rep("none", 3) ) {

    rgl.open()
    par3d(windowRect = size)
    par3d(mouseMode=mouseMode)
    bg3d("white")
    rgl.viewpoint(0,-65, fov=0, scale=c(1,1,1), zoom=1)

}
