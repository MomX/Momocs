\name{coo.rotatecenter}
\alias{coo.rotatecenter}
\title{Rotates shapes with a custom center}
\usage{
coo.rotatecenter(coo, theta, center = c(0, 0))
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Coo object.}

  \item{theta}{an angle, in radians.}

  \item{center}{the (x, y) position of the center}
}
\value{
a matrix of (x,y) coordinates or an Coo object.
}
\description{
rotates a shape of "theta" angles (in radians) and with a
(x, y) "center".
}
\examples{
data(bot)
coo.plot(bot[4])
coo.plot(coo.rotatecenter(bot[4], pi/2, c(0, 0)))
}
\keyword{coo_utilities}

