\name{coo.theta.pts}
\alias{coo.theta.pts}
\title{The angle of every edge of a shape}
\usage{
coo.theta.pts(coo, method = c("atan2", "acos")[1])
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates.}

  \item{method}{one of "atan2" or "acos" for a signed or
  not angle.}
}
\value{
numeric the angles in radians for every edge.
}
\description{
Returns the angle (in radians) of every edge of a shape,
}
\examples{
data(bot)
b <- coo.sample(bot[4], 64)
coo.theta.pts(b)
}
\keyword{coo_descriptors}

