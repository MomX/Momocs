\name{coo.convexity}
\alias{coo.convexity}
\title{Calculates the convexity of a shape}
\source{
Rosin PL. 2005. Computing global shape measures. Handbook
of Pattern Recognition and Computer Vision. 177-196.
}
\usage{
coo.convexity(coo)
}
\arguments{
  \item{coo}{a \code{matrix} of \eqn{(x; y)} coordinates.}
}
\value{
numeric, the convexity.
}
\description{
Calculated using a ratio of the eigen values (inertia axis)
}
\examples{
data(bot)
coo.convexity(bot[1])
}
\keyword{coo_descriptors}

