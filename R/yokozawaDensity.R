#' Crown profile function for the Yokozawa crown shape
#' @description more info
#' @param z
#' @param h
#' @param eta
#' @export
#' @examples
#' # Plot the crown profile function for Yokozawa's function:
#' z <- seq(0, 1, 0.01)
#' q <- yokozawaDensity(z, 1, 12)
#' plot(q, z, type = "l")
yokozawaDensity <- function(z, h, eta) {
    q <- 2 * eta * (1 - (z/h)^eta) * (z/h)^eta/z
    q[is.na(q)] <- 0
    return(q)
}
