\name{coo.theta3}
\alias{coo.theta3}
\title{The angle formed by three points.}
\usage{
coo.theta3(m, method = c("atan2", "acos")[1])
}
\arguments{
  \item{m}{a 3x2 matrix of 3 points (rows) and (x; y)
  coordinates}

  \item{method}{one of "atan2" or "acos" for a signed or
  not angle.}
}
\value{
numeric the angle in radians.
}
\description{
Returns the angle (in radians) defined by a triplet of
points
}
\examples{
data(bot)
b <- coo.sample(bot[4], 64)
b <- b[c(1, 14, 24), ]
coo.plot(b)
coo.theta3(b)
coo.theta3(b, method="acos")
}
\keyword{coo_descriptors}

