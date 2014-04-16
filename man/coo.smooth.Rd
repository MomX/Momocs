\name{coo.smooth}
\alias{coo.smooth}
\title{Smoothes coordinates}
\usage{
coo.smooth(coo, n)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Out object.}

  \item{n}{an integer to specify the number of smoothing
  iterations}
}
\value{
a matrix of (x,y) coordinates or an Out object.
}
\description{
Smoothes coordinates using a simple moving average. May be
useful to remove digitization noise.
}
\examples{
data(bot)
stack(bot)
stack(coo.smooth(bot, 10))
coo.plot(bot[4])
coo.plot(coo.smooth(bot[4], 30))
}
\keyword{coo_utilities}

