\name{coo.rectilinearity}
\alias{coo.rectilinearity}
\title{Calculates the rectilinearity of a shape}
\usage{
coo.rectilinearity(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates.}
}
\value{
a numeric, the rectilinearity
}
\description{
As proposed by by Zunic and Rosin
}
\note{
due to the laborious nature of the algorithm (in nb.pts^2),
and of its implementation, it may be very long to compute.
}
\examples{
data(bot)
b <- coo.sample(bot[4], 32)
coo.rectilinearity(b)
}
\keyword{coo_descriptors}

