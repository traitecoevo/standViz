\name{plotTree}
\alias{plotTree}
\title{Plot a single tree in 3D}
\usage{
  plotTree(X = 0, Y = 0, topHeight = 1,
    heightCrownBase = 0, crownWidth = 1, dbh = 0.01,
    crownShapeMatrix = NA, stemShapeMatrix = NA,
    crownShape = c("cone", "elipsoid", "ellipsoid", "round", "halfellipsoid", "paraboloid", "cylinder", "yokozawa"),
    eta = 13, crownColor = "forestgreen",
    stemColor = "brown", nz = 25, nalpha = 25, zvals = NA,
    ...)
}
\arguments{
  \item{X}{}

  \item{Y}{}

  \item{topHeight}{}

  \item{heightCrownBase}{}

  \item{crownWidth}{}

  \item{dbh}{}

  \item{crownShapeMatrix}{}

  \item{stemShapeMatrix}{}

  \item{crownShape}{One of "cone", "elipsoid", "ellipsoid",
  "round", "halfellipsoid", "paraboloid", "cylinder",
  "yokozawa"}

  \item{eta}{}

  \item{crownColor}{}

  \item{stemColor}{}

  \item{nz}{}

  \item{nalpha}{}

  \item{zvals}{}
}
\description{
  Plots a tree if available, uses stem and cronw shape
  Matrices provided, leading to faster computing times.
  Otherwise recalculates these.
}
\examples{
# some examples here.
standViz:::newRgl()
zvals <- c(0, seq(0.4, 0.9, by = 0.025), seq(0.91, 1, by = 0.01))
plotTree(crownShape = "yokozawa", crownWidth = 5, topHeight = 10, heightCrownBase = 0,
dbh = 0.1, eta = 13, zvals = zvals)
}
\seealso{
  \code{\link{plotStand}}
}

