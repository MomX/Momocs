\name{coo.close}
\alias{coo.close}
\title{Closes/'Uncloses' shapes}
\usage{
coo.close(coo)
}
\arguments{
  \item{coo}{a matrix of (x,y) coordinates or a Coo object}
}
\value{
a matrix of (x,y) coordinates or an Coo object.
}
\description{
Returns a closed shape from (un)closed shapes. See also
\link{coo.unclose}.
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

