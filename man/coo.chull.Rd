\name{coo.chull}
\alias{coo.chull}
\title{Calculates the convex hull of a shape}
\usage{
coo.chull(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates.}
}
\value{
a matrix of (x; y) coordinates defining the convex hull of
the shape.
}
\description{
Returns the ids of points that define the convex hull of a
shape.
}
\examples{
data(hearts)
h <- coo.sample(hearts[4], 32)
coo.plot(h)
ch <- coo.chull(h)
lines(ch, col="red", lty=2)
}
\keyword{coo_descriptors}

