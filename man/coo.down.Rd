\name{coo.down}
\alias{coo.down}
\title{Retains coordinates with negative y-coordinates}
\usage{
coo.down(coo)
}
\arguments{
  \item{coo}{either a \code{matrix} of \eqn{(x; y)}
  coordinates.}
}
\value{
a \code{matrix} of \eqn{(x; y)} coordinates.
}
\description{
Useful when shapes are aligned along the x-axis (e.g.
because of a bilateral symmetry) and when one wants to
retain just the lower side.
}
\examples{
data(bot)
b <- coo.alignxax(bot[1])
coo.plot(b)
coo.draw(coo.down(b), border="red")
}
\keyword{coo_utilities}

