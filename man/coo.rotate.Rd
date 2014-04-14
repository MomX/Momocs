\name{coo.rotate}
\alias{coo.rotate}
\title{Rotates coordinates}
\usage{
coo.rotate(coo, theta = 0)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Out object.}
}
\value{
a matrix of (x,y) coordinates or an Out object.
}
\description{
Rotates the coordinates by a 'theta' angle (in radians) If
not provided, assumed to be the centroid size. It involves
three steps: centering from current position, dividing
coordinates by 'scale', pusing back to the original
position.
}
\examples{
data(bot)
stack(bot)
stack(coo.rotate(bot, pi))
coo.plot(b[4])
coo.plot(coo.scale(b[4], -pi/2))
}
\seealso{
Other coo: \code{\link{coo.align}},
\code{\link{coo.align}}; \code{\link{coo.center}},
\code{\link{coo.center}}; \code{\link{coo.centpos}},
\code{\link{coo.centpos}}; \code{\link{coo.check}};
\code{\link{coo.close}}, \code{\link{coo.close}};
\code{\link{coo.sample.int}}, \code{\link{coo.sample.int}};
\code{\link{coo.sample}}, \code{\link{coo.sample.rr}};
\code{\link{coo.sample}}, \code{\link{coo.sample}};
\code{\link{coo.scale}}, \code{\link{coo.scale}};
\code{\link{coo.slide}}, \code{\link{coo.slide}};
\code{\link{coo.smooth}}, \code{\link{coo.smooth}};
\code{\link{coo.trans}}, \code{\link{coo.trans}};
\code{\link{is.closed}}, \code{\link{is.closed}}
}
\keyword{coo}

