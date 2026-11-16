## Crown geometry for drawing. The crown's shape is the model's, read from
## plant::canopy_profile(); only its size at a given leaf area density is
## decided here.

## The crown is drawn as a solid of revolution whose horizontal cross-section
## at height z holds the plant's leaf area at that height, q(z, H), at a fixed
## leaf area per crown volume `lad`. With q the density in relative height u =
## z / H, which integrates to one over the crown, its radius at u is
## crown_scale * sqrt(q(u)), and crown_scale is fixed by the crown holding all
## the plant's leaf area.
crown_scale <- function(area_leaf, height, lad = 1) {
  sqrt(area_leaf / (pi * lad * height))
}

## A species' crown, from the model: its outline as relative radius against u,
## drawn down to the height that holds 95% of the leaf area (below it the
## profile is a long, near-empty tail that reads as a funnel); its widest
## relative radius; and eta_c, the leaf-area-weighted mean of u in the Yokozawa
## profile. `control` is the run's, which sets the shading model.
species_crown <- function(strategy, control = NULL, n = 33) {
  fine <- plant::canopy_profile(strategy, seq(0, 1, by = 0.0005), control)
  p <- fine$profile
  ## Q is the leaf area above u, falling from 1 to 0.
  u_base <- p$u[max(which(p$Q >= 0.95))]
  u <- u_base + (1 - u_base) * (1 - cos(seq(0, pi, length.out = n))) / 2
  outline <- plant::canopy_profile(strategy, u, control)$profile
  list(profile = list(u = c(u_base, u), r = c(0, sqrt(outline$q))),
       widest = sqrt(max(p$q)), eta_c = fine$eta_c)
}
