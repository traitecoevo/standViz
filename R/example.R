# ----------------------------------------------------------------------------
# plot leaf density distribution from yokozawa function
z <- seq(0, 1, 0.01)
q <- yokozawaDensity(z, 1, 12)
plot(q, z, type = "l")
newRgl()
zvals <- c(0, seq(0.4, 0.9, by = 0.025), seq(0.91, 1, by = 0.01))
plotTree(crownShape = "yokozawa", crownWidth = 5, topHeight = 10, heightCrownBase = 0,
    dbh = 0.1, eta = 13, zvals = zvals)
# Define site
siteData <- NULL
siteData$x0 <- 0
siteData$xmax <- 10
siteData$y0 <- 0
siteData$ymax <- 10
siteData$area <- (siteData$xmax - siteData$x0) * (siteData$ymax - siteData$y0)
rgl.clear()
noTrees <- 10
treeData <- data.frame(topHeight = rep(20, times = noTrees), heightCrownBase = rep(0,
    times = noTrees), crownWidth = rep(10, times = noTrees), dbh = rep(0.5, times = noTrees),
    crownShape = "yokozawa", eta = 10)
plotStand(siteData, treeData, verbose = F)
