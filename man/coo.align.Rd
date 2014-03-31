\name{coo.align}
\alias{coo.align}
\title{Aligns coordinates}
\usage{
coo.align(coo)
}
\arguments{
  \item{coo}{a matrix of (x,y) coordinates or a \code{Out}
  object.}
}
\value{
a matrix of (x,y) coordinates or a \code{Out} object.
}
\description{
Aligns the coordinates along their longer axis using
var-cov matrix and eigen values.
}
\examples{
coo.align(matrix(1:10, ncol=2))
}
\seealso{
Other coo: \code{\link{coo.center}},
\code{\link{coo.center}}; \code{\link{coo.check}};
\code{\link{coo.rotate}}, \code{\link{coo.rotate}};
\code{\link{coo.scale}}, \code{\link{coo.scale}};
\code{\link{coo.trans}}, \code{\link{coo.trans}};
\code{\link{is.closed}}, \code{\link{is.closed}}
}
\keyword{coo}

