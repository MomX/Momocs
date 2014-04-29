\name{coo.chull}
\alias{coo.chull}
\title{Calculates the convex hull of a shape}
\usage{
coo.chull(coo)
}
\arguments{
  \item{coo}{a \code{matrix} of \eqn{(x; y)} coordinates.}
}
\value{
a \code{matrix} of ids defining the convex hull of the
shape.
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

