\name{coo.sample.rr}
\alias{coo.sample.rr}
\title{Samples coordinates (regular radius)}
\usage{
coo.sample.rr(coo, n)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Out object.}

  \item{n}{an integer, the number fo points to sample.}
}
\value{
a matrix of (x,y) coordinates or an Out object.
}
\description{
Samples n coordinates with a regular angle
}
\examples{
data(bot)
stack(bot)
stack(coo.sample.rr(bot, 12))
coo.plot(bot[4])
coo.plot(rr <- coo.sample.rr(bot[4], 12))
segments(coo.centpos(bot[4][1], coo.centpos(bot[4])[2]), rr[, 1], rr[, 2])
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
\code{\link{coo.sample.int}}; \code{\link{coo.sample}},
\code{\link{coo.sample}}; \code{\link{coo.scale}},
\code{\link{coo.scale}}; \code{\link{coo.slide}},
\code{\link{coo.slide}}; \code{\link{coo.smooth}},
\code{\link{coo.smooth}}; \code{\link{coo.trans}},
\code{\link{coo.trans}}; \code{\link{is.closed}},
\code{\link{is.closed}}
}
\keyword{coo}

