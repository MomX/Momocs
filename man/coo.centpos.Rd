\name{coo.centpos}
\alias{coo.centpos}
\title{Returns the position of the centroid}
\usage{
coo.centpos(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Coo object.}
}
\value{
a numeric or a matrix of (x,y) coordinates.
}
\description{
Returns the (x, y) centroid coordinates of a shape.
}
\examples{
data(bot)
stack(bot)
stack(coo.smooth(bot, 10))
coo.plot(bot[4])
coo.plot(coo.smooth(bot[4], 30))
}
\keyword{coo_utilities}

