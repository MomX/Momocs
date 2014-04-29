\name{coo.rectilinearity}
\alias{coo.rectilinearity}
\title{Calculates the rectilinearity of a shape}
\source{
Zunic J, Rosin PL. 2003. Rectilinearity measurements for
polygons. IEEE Transactions on Pattern Analysis and Machine
Intelligence 25: 1193-1200.
}
\usage{
coo.rectilinearity(coo)
}
\arguments{
  \item{coo}{a \code{matrix} of \eqn{(x; y)} coordinates.}
}
\value{
numeric, the rectilinearity
}
\description{
As proposed by Zunic and Rosin (see below). I do not 100%
guarantee the code.
}
\note{
due to the laborious nature of the algorithm (in nb.pts^2),
and of its implementation, it may be very long to compute.
}
\examples{
data(bot)
b <- coo.sample(bot[1], 32)
coo.rectilinearity(b)
}
\keyword{coo_descriptors}

