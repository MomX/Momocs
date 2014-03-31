\name{coo.rotate}
\alias{coo.rotate}
\title{Rotates coordinates}
\usage{
coo.rotate(coo, theta = 0)
}
\arguments{
  \item{coo}{a matrix of (x,y) coordinates or a \code{Out}
  object.}
}
\value{
a matrix of (x,y) coordinates or a \code{Out} object.
}
\description{
Rotates the coordinates by a 'theta' angle (in radians) If
not provided, assumed to be the centroid size. It involves
three steps: centering from current position, dividing
coordinates by 'scale', pusing back to the original
position.
}
\examples{
coo.rotate(matrix(1:10, ncol=2))
}
\seealso{
Other coo: \code{\link{coo.align}},
\code{\link{coo.align}}; \code{\link{coo.center}},
\code{\link{coo.center}}; \code{\link{coo.check}};
\code{\link{coo.scale}}, \code{\link{coo.scale}};
\code{\link{coo.trans}}, \code{\link{coo.trans}};
\code{\link{is.closed}}, \code{\link{is.closed}}
}
\keyword{coo}

