\name{coo.calliper}
\alias{coo.calliper}
\title{Calculates the calliper length}
\usage{
coo.calliper(coo, arr.ind = FALSE)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates.}

  \item{arr.ind}{a boolean, if provided returns a list with
  the calliper length ($length) and the two points
  ($arr.ind)}
}
\value{
a numeric, the centroid size.
}
\description{
Also called the Feret's diameter, the longest distance
between two points of the shape provided.
}
\examples{
data(bot)
b <- bot[4]
coo.calliper(b)
p <- coo.calliper(b, arr.ind=TRUE)
p$length
ids <- p$arr.ind
coo.plot(b)
segments(b[ids[1], 1], b[ids[1], 2], b[ids[2], 1], b[ids[2], 2], lty=2)
}
\keyword{coo_descriptors}

