\name{coo.center}
\alias{coo.center}
\title{Centers coordinates}
\usage{
coo.center(coo)
}
\arguments{
  \item{coo}{a matrix of (x,y) coordinates or a list.}
}
\value{
a matrix of (x,y) coordinates.
}
\description{
Returns a shape centered on the origin.
}
\examples{
coo.center(matrix(1:10, ncol=2))
coo.center(list(x=1:5, y=6:10))
}
\seealso{
Other coo: \code{\link{coo.align}},
\code{\link{coo.align}}; \code{\link{coo.check}};
\code{\link{coo.rotate}}, \code{\link{coo.rotate}};
\code{\link{coo.scale}}, \code{\link{coo.scale}};
\code{\link{coo.trans}}, \code{\link{coo.trans}};
\code{\link{is.closed}}, \code{\link{is.closed}}
}
\keyword{coo}

