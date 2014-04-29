\name{coo.down}
\alias{coo.down}
\title{Retains coordinates with negative y-coordinates}
\usage{
coo.down(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates.}
}
\value{
a matrix of (x,y) coordinates.
}
\description{
Useful when shapes are aligned along the x-axis (e.g.
because of a bilateral symmetry) and when one wants to
retain just the lower side.
}
\examples{
data(bot)
b <- coo.alignxax(bot[4])
coo.plot(b)
coo.plot(coo.down(b))
}
\keyword{coo_utilities}

