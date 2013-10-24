\name{yokozawaDensity}
\alias{yokozawaDensity}
\title{Crown profile function for the Yokozawa crown shape}
\usage{
  yokozawaDensity(z, h, eta)
}
\arguments{
  \item{z}{}

  \item{h}{}

  \item{eta}{}
}
\description{
  more info
}
\examples{
# Plot the crown profile function for Yokozawa's function:
z <- seq(0, 1, 0.01)
q <- yokozawaDensity(z, 1, 12)
plot(q, z, type = "l")
}

