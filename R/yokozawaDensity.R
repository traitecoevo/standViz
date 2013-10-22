yokozawaDensity <- function(z, h, eta) {
    q <- 2 * eta * (1 - (z/h)^eta) * (z/h)^eta/z
    q[is.na(q)] <- 0
    return(q)
}
