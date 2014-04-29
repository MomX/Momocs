\name{is.closed}
\alias{is.closed}
\title{Tests if shapes are closed}
\usage{
is.closed(coo)
}
\arguments{
  \item{coo}{a matrix of (x,y) coordinates or a Coo object}
}
\value{
a boolean.
}
\description{
Returns TRUE/FALSE whether the last coordinate of the
shapes is the same as the first one.
}
\examples{
is.closed(matrix(1:10, ncol=2))
is.closed(coo.close(matrix(1:10, ncol=2)))
}
\keyword{coo_utilities}

