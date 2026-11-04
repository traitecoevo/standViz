## Tree geometry from a strategy's own allometry, so the picture is the model.
## FF16 only so far; each formula mirrors plant's ff16_strategy.h and a test
## checks it against plant's Individual.

#' FF16 tree geometry from height
#'
#' Leaf area, stem diameter and crown size of an FF16 plant, from the strategy's
#' allometry. The crown is drawn as a solid of revolution whose horizontal
#' cross-section at height `z` holds the plant's leaf area at that height
#' (the Yokozawa leaf density `q(z, H)`) at a fixed leaf area per crown volume
#' `lad`. So the radius at relative height `u = z / H` is
#' `crown_scale * crown_profile(u, eta)`.
#'
#' @param height Plant height (m).
#' @param area_heartwood Heartwood area (m^2), from the plant run.
#' @param pars An FF16 strategy's `pars` list (needs `a_l1`, `a_l2`, `theta`,
#'   `a_b1`).
#' @param lad Leaf area per crown volume (m^2 m^-3): the one visual parameter.
#' @return A list of `area_leaf`, `stem_diameter` and `crown_scale` (m), each
#'   the length of `height`.
#' @export
ff16_geometry <- function(height, area_heartwood, pars, lad = 1) {
  area_leaf <- (height / pars$a_l1)^(1 / pars$a_l2)
  area_sapwood <- pars$theta * area_leaf
  area_stem <- area_sapwood * (1 + pars$a_b1) + area_heartwood
  list(area_leaf = area_leaf,
       stem_diameter = sqrt(4 * area_stem / pi),
       crown_scale = sqrt(area_leaf / (pi * lad * height)))
}

#' Relative crown radius at relative height
#'
#' `sqrt(2 eta (1 - u^eta) u^(eta - 1))`: the Yokozawa leaf density in relative
#' height `u = z / H`, square-rooted to give a radius. Multiply by
#' [ff16_geometry()]'s `crown_scale` to get metres.
#'
#' @param u Relative height, in `[0, 1]`.
#' @param eta Crown shape parameter.
#' @export
crown_profile <- function(u, eta) {
  sqrt(2 * eta * (1 - u^eta) * u^(eta - 1))
}

## Leaf-area-weighted mean relative height of the crown, as plant's eta_c().
crown_centre <- function(eta) {
  1 - 2 / (1 + eta) + 1 / (1 + 2 * eta)
}
