\name{coo.bookstein}
\alias{coo.bookstein}
\title{Register Bookstein's coordinates}
\usage{
coo.bookstein(coo, ldk1, ldk2)
}
\arguments{
  \item{coo}{either a \code{matrix} of \eqn{(x; y)}
  coordinates, or a \link{Coo} object.}

  \item{ldk1}{the id of the first point of the new
  baseline}

  \item{ldk2}{the id of the second point of the new
  baseline}
}
\value{
a \code{matrix} of \eqn{(x; y)} coordinates, or a
\link{Coo} object.
}
\description{
Registers a new baseline for the shape, with the
\code{ldk1}-th and \code{ldk2}-th points being set on
\eqn{(x= -0.5; y=0)} and \eqn{(x= 0.5; y=0)}, respectively.
}
\examples{
data(hearts)
stack(hearts)
stack(coo.bookstein(hearts, 2, 4))
h <- hearts[1]
coo.plot(h)
coo.plot(coo.bookstein(h, 20, 57), border="red")
}
\seealso{
\link{coo.baseline}
}
\keyword{coo_utilities}

