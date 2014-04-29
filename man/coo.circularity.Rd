\name{coo.circularity}
\alias{coo.circularity}
\alias{coo.compactness}
\alias{coo.shapefactor}
\title{Calculates the circularity of a shape}
\usage{
coo.circularity(coo)
}
\arguments{
  \item{coo}{a \code{matrix} of \eqn{(x; y)} coordinates.}
}
\value{
numeric, the circularity.
}
\description{
Returns the "circularity measure". Also called compactness
}
\examples{
data(bot)
coo.circularity(bot[1])
}
\keyword{coo_descriptors}

