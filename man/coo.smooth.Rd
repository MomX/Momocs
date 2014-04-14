\name{coo.smooth}
\alias{coo.smooth}
\title{Smoothes coordinates}
\usage{
coo.smooth(coo, n)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Out object.}

  \item{n}{an integer to specify the number of smoothing
  iterations}
}
\value{
a matrix of (x,y) coordinates or an Out object.
}
\description{
Smoothes coordinates using a simple moving average. May be
useful to remove digitization noise.
}
\examples{
data(bot)
stack(bot)
stack(coo.smooth(bot, 10))
coo.plot(bot[4])
coo.plot(coo.smooth(bot[4], 30))
}
\seealso{
Other coo: \code{\link{coo.align}},
\code{\link{coo.align}}; \code{\link{coo.bookstein}},
\code{\link{coo.bookstein}}; \code{\link{coo.center}},
\code{\link{coo.center}}; \code{\link{coo.centpos}},
\code{\link{coo.centpos}}; \code{\link{coo.check}};
\code{\link{coo.close}}, \code{\link{coo.close}};
\code{\link{coo.perim.pts}}, \code{\link{coo.perim.pts}};
\code{\link{coo.rotatecenter}},
\code{\link{coo.rotatecenter}}; \code{\link{coo.rotate}},
\code{\link{coo.rotate}}; \code{\link{coo.sample.int}},
\code{\link{coo.sample.int}}; \code{\link{coo.sample.rr}},
\code{\link{coo.sample.rr}}; \code{\link{coo.sample}},
\code{\link{coo.sample}}; \code{\link{coo.scale}},
\code{\link{coo.scale}}; \code{\link{coo.slide}},
\code{\link{coo.slide}}; \code{\link{coo.trans}},
\code{\link{coo.trans}}; \code{\link{is.closed}},
\code{\link{is.closed}}
}
\keyword{coo}

