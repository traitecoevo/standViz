# gather items with same color and plot
# want to minimise number of calls to rgl

plot.standVizStand <- function(x,add=FALSE, size=c(1, 1, 500, 500), ...) {

	if(!add)
		newRgl(size=size)

	crownColors <- sapply(x, function(myTree) myTree$crownColor)
	stemColors <- sapply(x, function(myTree) myTree$stemColor)

	getItem <- function(n, myStand, v="crown") myStand[[n]][[v]]

	for(i in unique(crownColors))
		plot3dShape(
			do.call(rbind,
				lapply(which(i == crownColors), getItem, myStand=x)), col = i)

	for(i in unique(stemColors))
		plot3dShape(
			do.call(rbind,
				lapply(which(i == stemColors), getItem, myStand=x, v="stem")), col = i)
}
