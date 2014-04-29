\name{coo.eccentricity.eigen}
\alias{coo.eccentricity.eigen}
\title{Returns the eccentricity (eigenvalues) of a shape}
\source{
Rosin PL. 2005. Computing global shape measures. Handbook
of Pattern Recognition and Computer Vision. 177-196.
}
\usage{
coo.eccentricity.eigen(coo)
}
\arguments{
  \item{coo}{a \code{matrix} of \eqn{(x; y)} coordinates.}
}
\value{
numeric, the eccentricity (eigenvalues)
}
\description{
Calculated using a ratio of the eigen values (inertia axis)
}
\examples{
data(bot)
coo.eccentricity.eigen(bot[1])
}
\keyword{coo_descriptors}

