\name{coo.align}
\alias{coo.align}
\title{Aligns coordinates}
\usage{
coo.align(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Out object.}
}
\value{
a matrix of (x,y) coordinates or an Out object.
}
\description{
Aligns the coordinates along their longer axis using
var-cov matrix and eigen values.
}
\examples{
data(bot)
stack(bot)
stack(coo.align(bot))
coo.plot(bot[4])
coo.plot(coo.align(bot[4]))
}
\keyword{coo_utilities}

