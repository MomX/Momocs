\name{coo.circularity.norm}
\alias{coo.circularity.norm}
\title{Calculates the "normalized" circularity of a shape}
\usage{
coo.circularity.norm(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates.}
}
\value{
a numeric, the circularity normalized to the unit circle.
}
\description{
Returns the "circularity", also called compactness
}
\examples{
data(bot)
coo.circularity.norm(bot[4])
}
\keyword{coo_descriptors}

