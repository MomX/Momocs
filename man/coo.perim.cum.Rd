\name{coo.perim.cum}
\alias{coo.perim.cum}
\title{Calculates the cumulative distance between every points of a shape.}
\usage{
coo.perim.cum(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates.}
}
\value{
numeric the cumulated sum for coo.perim.pts.
}
\description{
Just a wrapper for cumsum(coo.perim.pts). See
\link{coo.perim.pts}.
}
\examples{
data(bot)
b <- coo.sample(bot[4], 24)
coo.perim.cum(b)
}
\keyword{coo_descriptors}

