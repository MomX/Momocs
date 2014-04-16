\name{coo.circularity}
\alias{coo.circularity}
\alias{coo.compactness}
\alias{coo.shapefactor}
\title{Calculates the circularity of a shape}
\usage{
coo.circularity(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates.}
}
\value{
a numeric, the circularity.
}
\description{
Returns the "circularity measure". Also called compactness
}
\examples{
data(bot)
coo.circularity(bot[4])
}
\keyword{coo_descriptors}

