\name{coo.unclose}
\alias{coo.unclose}
\title{'Uncloses' shapes}
\usage{
coo.unclose(coo)
}
\arguments{
  \item{coo}{a matrix of (x,y) coordinates or a Out object}
}
\value{
a matrix of (x,y) coordinates or an Out object.
}
\description{
Returns a unclosed shape from (un)closed shapes. See also
\link{coo.close}.
}
\examples{
x <- (matrix(1:10, ncol=2))
x2 <- coo.close(x)
x3 <- coo.unclose(x2)
x
is.closed(x)
x2
is.closed(x2)
x3
is.closed(x3)
}
\keyword{coo_utilities}

