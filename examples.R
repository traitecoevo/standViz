library(trees3D)

s <- tree_shape3d()
rgl::triangles3d(s, col="forestgreen")

tree <- tree3d()
plot(tree)

H <- 1:10
trees <- data.frame(height=H, x=1:10, y=1, height_crown_base=0.5*H,
                            crown_width=0.3*H, dbh=0.1*H)

stand <- stand3d(trees)
plot(stand)

# How to create new rgl objects
new_rgl()
