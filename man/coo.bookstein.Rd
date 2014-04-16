\name{coo.bookstein}
\alias{coo.bookstein}
\title{Register Bookstein's coordinates}
\usage{
coo.bookstein(coo, ldk1, ldk2)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Out object.}

  \item{ldk1}{the id of the first point of the new
  baseline}

  \item{ldk2}{the id of the second point of the new
  baseline}
}
\value{
a matrix of (x,y) coordinates or an Out object.
}
\description{
Registers a new baseline for the shape, with the ldk1-th
}
\examples{
data(hearts)
stack(hearts)
stack(coo.bookstein(hearts, 2, 4))
}
\keyword{coo_utilities}

