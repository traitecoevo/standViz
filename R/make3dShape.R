# Returns a 3D shapeMatrix suitable for passing to rgl plot.triangles (use
# plot3dShape) This object has height 1, width 1, and is located at (0,0,0). use
# the function resizeShape to scale and move to a different set of coordinates
make3dShape <- function(shape = c("cone", "ellipsoid", "halfellipsoid", "paraboloid",
    "cylinder", "yokozawa"), nz = 25, nalpha = 25, zvals = NA, eta = 4) {
    # Canopy shape
    shape <- match.arg(shape)
    # Define canopy shape function. Gives relative distance from centre at different
    # relative heights within the crown
    if (shape == "cone")
        distfun <- function(z) (1 - z)
    if (shape == "ellipsoid")
        distfun <- function(z) sqrt(1 - ((z - 1/2)^2)/((1/2)^2))
    if (shape == "halfellipsoid")
        distfun <- function(z) sqrt(1 - z^2)
    if (shape == "paraboloid")
        distfun <- function(z) sqrt(1 - z)
    if (shape == "cylinder")
        distfun <- function(z) 1
    if (shape == "yokozawa") {
        distfun <- function(z) yokozawaDensity(z, 1, eta)
    }
    # Heights to calculate points, if not already defined
    if (is.na(zvals[1]))
        zvals <- seq(1, 0, length = nz) else nz <- length(zvals)
    # Make matrix of x,y,z values, by choosing points with given radius and angle at
    # a given height
    radius <- rep(0.5 * distfun(zvals)/max(distfun(zvals)), times = nalpha)  #scaled to specified object width
    angles <- rep(seq(0, 2 * pi, length = nalpha), each = nz)
    x <- radius * cos(angles)
    y <- radius * sin(angles)
    z <- rep(zvals, times = nalpha)
    m <- matrix(cbind(x, y, z), ncol = 3)
    # determine ordering of points to make triangles. Each row of tm gives indices
    # for the vertices of a single triangle
    # OLD VERSION - fits convex hull around surface. BUt this doesn't work for
    # inverted shapes require(geometry, quietly=TRUE) tm <-
    # try(t(geometry::surf.tri(m, geometry::delaunayn(m))))}
    # NEW VERSION traverse each vertical spines and define two triangles between each
    # set of four points (adjacent splines and heights)
    tm <- matrix(nrow = 3, ncol = (nalpha - 1) * 2 * (nz - 1))
    r <- 1
    for (ai in 1:(nalpha - 1)) {
        # do for all angles do for each height
        for (zi in 1:(nz - 1)) {
            # first (upper) triangle
            tm[1, r] <- (ai - 1) * nz + zi
            tm[2, r] <- (ai) * nz + zi
            tm[3, r] <- (ai) * nz + zi + 1
            # second (lower) triangle
            tm[1, r + 1] <- (ai - 1) * nz + zi
            tm[2, r + 1] <- (ai - 1) * nz + zi + 1
            tm[3, r + 1] <- (ai) * nz + zi + 1
            # increment index
            r <- r + 2
        }
    }
    # return matrix of triangle cooridnates (note, includes duplicate points because
    # each used in multiple triangles)
    return(m[tm, 1:3])
}
