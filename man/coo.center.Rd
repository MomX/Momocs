\name{coo.center}
\alias{coo.center}
\title{Centers coordinates}
\usage{
coo.center(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Out object.}
}
\value{
a matrix of (x,y) coordinates or an Out object.
}
\description{
Returns a shape centered on the origin.
}
\examples{
coo.center(matrix(1:10, ncol=2))
coo.center(list(x=1:5, y=6:10))
}
\keyword{coo_utilities}

