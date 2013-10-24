\name{plotStand}
\alias{plotStand}
\title{Plot a stand in 3D}
\usage{
  plotStand(siteData, treeData, verbose = FALSE, nz = 25,
    nalpha = 25, zvals = NA, ...)
}
\arguments{
  \item{siteData}{blah}

  \item{treeData}{blah}

  \item{verbose}{Logical}

  \item{nz}{Number of 3D triangles in the Z direction (?)}

  \item{nalpha}{Number of angles (...)}

  \item{zvals}{blah}

  \item{\dots}{Further arguments (currently ignored)}
}
\description{
  More details on what it does
}
\examples{
# Example stand plot
siteData <- list(x0=0,
xmax=10,
y0=0,
ymax=10)
siteData$area <- with(siteData, (xmax - x0) * (ymax - y0))

standViz:::newRgl()
noTrees <- 10
treeData <- data.frame(topHeight = rep(20, times = noTrees),
heightCrownBase = rep(0,times = noTrees),
crownWidth = rep(10, times = noTrees),
dbh = rep(0.5, times = noTrees),
crownShape = "yokozawa", eta = 10)
plotStand(siteData, treeData, verbose = FALSE)
}

