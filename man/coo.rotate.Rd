\name{coo.rotate}
\alias{coo.rotate}
\title{Rotates coordinates}
\usage{
coo.rotate(coo, theta = 0)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Out object.}

  \item{theta}{angle in radians}
}
\value{
a matrix of (x,y) coordinates or an Out object.
}
\description{
Rotates the coordinates by a 'theta' angle (in radians) If
not provided, assumed to be the centroid size. It involves
three steps: centering from current position, dividing
coordinates by 'scale', pusing back to the original
position.
}
\examples{
data(bot)
stack(bot)
stack(coo.rotate(bot, pi))
coo.plot(bot[4])
coo.plot(coo.rotate(bot[4], -pi/2))
}
\keyword{coo_utilities}

