\name{coo.eccentricity.eigen}
\alias{coo.eccentricity.eigen}
\title{Returns the eccentricity (eigenvalues) of a shape}
\usage{
coo.eccentricity.eigen(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates.}
}
\value{
a numeric, the eccentricity (eigenvalues)
}
\description{
Calculated using a ratio of the eigen values (inertia axis)
}
\examples{
data(bot)
coo.eccentricity.eigen(bot[4])
}
\keyword{coo_descriptors}

