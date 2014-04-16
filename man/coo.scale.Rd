\name{coo.scale}
\alias{coo.scale}
\title{Scales coordinates}
\usage{
coo.scale(coo, scale)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Out object.}

  \item{scale}{numeric, the scaling factor, by default the
  centroid size.}
}
\value{
a matrix of (x,y) coordinates or an Out object.
}
\description{
Scales the coordinates by a 'scale' factor. If not
provided, assumed to be the centroid size. It involves
three steps: centering from current position, dividing
coordinates by 'scale', pusing back to the original
position.
}
\examples{
data(bot)
stack(bot)
stack(coo.scale(bot))
coo.plot(bot[4])
coo.plot(coo.scale(bot[4]))
}
\keyword{coo_utilities}

