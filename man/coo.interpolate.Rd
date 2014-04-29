\name{coo.interpolate}
\alias{coo.interpolate}
\title{Interpolates coordinates}
\usage{
coo.interpolate(coo, n)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Coo object.}

  \item{n}{an integer, the number fo points to
  interpolate.}
}
\value{
a matrix of (x,y) coordinates or an Coo object.
}
\description{
Interpolates n coordinates "among existing points"between"
existing points, along the perimeter of the coordinates
provided and keeping the first point
}
\examples{
data(bot)
stack(bot)
stack(coo.interpolate(coo.sample(bot, 12), 120))
coo.plot(bot[4])
coo.plot(coo.interpolate(coo.sample(bot[4], 12), 120))
}
\keyword{coo_utilities}

