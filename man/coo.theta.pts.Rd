\name{coo.theta.pts}
\alias{coo.theta.pts}
\title{The angle of every edge of a shape}
\usage{
coo.theta.pts(coo, method = c("atan2", "acos")[1])
}
\arguments{
  \item{coo}{a \code{matrix} or a list of \eqn{(x; y)}
  coordinates.}

  \item{method}{one of "atan2" or "acos" for a signed or
  not angle.}
}
\value{
\code{numeric} the angles in radians for every edge.
}
\description{
Returns the angle (in radians) of every edge of a shape,
}
\examples{
data(bot)
b <- coo.sample(bot[1], 64)
coo.theta.pts(b)
}
\keyword{coo_descriptors}

