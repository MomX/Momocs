\name{coo.solidity}
\alias{coo.solidity}
\title{Calculates the solidity of a shape}
\usage{
coo.solidity(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates.}
}
\value{
a numeric, the solidity of a shape.
}
\description{
Returns the ids of points that define the convex hull of a
shape.
}
\examples{
data(bot)
coo.solidity(bot[4])
}
\keyword{coo_descriptors}

