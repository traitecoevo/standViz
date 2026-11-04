## Crown geometry for drawing. Leaf area and stem diameter come from plant
## (plant::expand_state()); only the crown's shape and size at a given leaf area
## density are decided here.

#' Crown size from leaf area
#'
#' The crown is drawn as a solid of revolution whose horizontal cross-section at
#' height `z` holds the plant's leaf area at that height (the Yokozawa leaf
#' density `q(z, H)`) at a fixed leaf area per crown volume `lad`. Its radius at
#' relative height `u = z / H` is then `crown_scale * crown_profile(u, eta)`.
#'
#' @param area_leaf Leaf area (m^2).
#' @param height Plant height (m).
#' @param lad Leaf area per crown volume (m^2 m^-3): the one visual parameter.
#' @return The crown scale (m).
#' @export
crown_scale <- function(area_leaf, height, lad = 1) {
  sqrt(area_leaf / (pi * lad * height))
}

#' Relative crown radius at relative height
#'
#' `sqrt(2 eta (1 - u^eta) u^(eta - 1))`: the Yokozawa leaf density in relative
#' height `u = z / H`, square-rooted to give a radius. Multiply by
#' [crown_scale()] to get metres.
#'
#' @param u Relative height, in `[0, 1]`.
#' @param eta Crown shape parameter.
#' @export
crown_profile <- function(u, eta) {
  sqrt(2 * eta * (1 - u^eta) * u^(eta - 1))
}

## Widest relative radius of the crown, for each eta.
crown_widest <- function(eta) {
  vapply(eta, function(e) max(crown_profile(seq(0, 1, by = 0.001), e)), 0)
}

## Leaf-area-weighted mean relative height of the crown, as plant's eta_c().
crown_centre <- function(eta) {
  1 - 2 / (1 + eta) + 1 / (1 + 2 * eta)
}
