\name{coo.scale}
\alias{coo.scale}
\title{Scales coordinates}
\usage{
coo.scale(coo, scale)
}
\arguments{
  \item{coo}{a matrix of (x,y) coordinates or a list.}
}
\value{
a matrix of (x,y) coordinates.
}
\description{
Scales the coordinates by a 'scale' factor. If not
provided, assumed to be the centroid size. It involves
three steps: centering from current position, dividing
coordinates by 'scale', pusing back to the original
position.
}
\examples{
coo.scale(matrix(1:10, ncol=2))
}
\seealso{
Other coo: \code{\link{coo.align}},
\code{\link{coo.align}}; \code{\link{coo.center}},
\code{\link{coo.center}}; \code{\link{coo.check}};
\code{\link{coo.rotate}}, \code{\link{coo.rotate}};
\code{\link{coo.trans}}, \code{\link{coo.trans}};
\code{\link{is.closed}}, \code{\link{is.closed}}
}
\keyword{coo}

