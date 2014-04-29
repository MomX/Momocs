\name{coo.up}
\alias{coo.up}
\title{Retains coordinates with positive y-coordinates}
\usage{
coo.up(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates.}
}
\value{
a matrix of (x,y) coordinates.
}
\description{
Useful when shapes are aligned along the x-axis (e.g.
because of a bilateral symmetry) #' and when one wants to
retain just the upper side.
}
\examples{
data(bot)
b <- coo.alignxax(bot[4])
coo.plot(b)
coo.plot(coo.up(b))
}
\keyword{coo_utilities}

