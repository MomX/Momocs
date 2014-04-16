\name{coo.convexity}
\alias{coo.convexity}
\title{Calculates the convexity of a shape}
\usage{
coo.convexity(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates.}
}
\value{
a numeric, the convexity.
}
\description{
Calculated using a ratio of the eigen values (inertia axis)
}
\examples{
data(bot)
coo.convexity(bot[4])
}
\keyword{coo_descriptors}

