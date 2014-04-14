\name{coo.centpos}
\alias{coo.centpos}
\title{Returns the position of the centroid}
\usage{
coo.centpos(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Out object.}
}
\value{
a numeric or a matrix of (x,y) coordinates.
}
\description{
Returns the (x, y) centroid coordinates of a shape.
}
\examples{
data(bot)
stack(bot)
stack(coo.smooth(bot, 10))
coo.plot(b[4])
coo.plot(coo.smooth(b[4], 30))
}
\seealso{
Other coo: \code{\link{coo.align}},
\code{\link{coo.align}}; \code{\link{coo.center}},
\code{\link{coo.center}}; \code{\link{coo.check}};
\code{\link{coo.close}}, \code{\link{coo.close}};
\code{\link{coo.rotate}}, \code{\link{coo.rotate}};
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

