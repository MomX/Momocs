\name{coo.perim.pts}
\alias{coo.perim.pts}
\title{Calculates the (cumulative) distance between every points of a shape.}
\usage{
coo.perim.pts(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates.}
}
\value{
a numeric with every distance for coo.perim.pts.
}
\description{
Calculates the euclidean distance between every points of a
shape for coo.perim.pts. The cumulative sum for
coo.perim.cum
}
\examples{
data(bot)
b <- coo.sample(bot[4], 24)
coo.perim.pts(b)
}
\keyword{coo_descriptors}

