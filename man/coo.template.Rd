\name{coo.template}
\alias{coo.template}
\title{"Templates" list and matrix of coordinates.}
\usage{
coo.template(coo, size)
}
\arguments{
  \item{coo}{A \code{list} or a \code{matrix} of
  coordinates.}

  \item{size}{\code{numeric}. Indicates the length of the
  side "inscribing" the shape.}
}
\value{
Returns a matrix of \code{(x; y)}coordinates.
}
\description{
\code{coo.template} returns \code{coo} so that the shape it
is centered on the origin and inscribed in a size-side
square, also centered on the origin; see
\link{coo.list.panel} for an illustration of this function.
}
\examples{
data(bot)
coo <- bot[1]
coo.plot(coo.template(coo), xlim=c(-1, 1), ylim=c(-1, 1))
rect(-0.5, -0.5, 0.5, 0.5)

s <- 0.01
coo.plot(coo.template(coo, s))
rect(-s/2, -s/2, s/2, s/2)
}
\seealso{
\link{coo.list.panel}.
}
\keyword{Utilities}

