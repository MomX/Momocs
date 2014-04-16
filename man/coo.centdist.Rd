\name{coo.centdist}
\alias{coo.centdist}
\title{Returns the distance between everypoints and the centroid
For every point of the shape, returns the (centroid-points) distance.}
\usage{
coo.centdist(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates.}
}
\value{
a matrix of (x,y) coordinates.
}
\description{
Returns the distance between everypoints and the centroid
For every point of the shape, returns the (centroid-points)
distance.
}
\examples{
data(bot)
b <- coo.sample(bot[4], 64)
d <- coo.centdist(b)
barplot(d)
}
\keyword{coo_descriptors}

