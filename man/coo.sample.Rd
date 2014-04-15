\name{coo.sample}
\alias{coo.sample}
\title{Sample coordinates (among points)}
\usage{
coo.sample(coo, n)
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
Sample n coordinates among existing points
}
\examples{
data(bot)
stack(bot)
stack(coo.sample(bot, 12))
coo.plot(bot[4])
coo.plot(coo.sample(bot[4], 12))
}
\seealso{
Other coo: \code{\link{coo.align.xax}};
\code{\link{coo.align}}, \code{\link{coo.align}};
\code{\link{coo.area}}, \code{\link{coo.area}};
\code{\link{coo.baseline}}, \code{\link{coo.baseline}};
\code{\link{coo.bookstein}}, \code{\link{coo.bookstein}};
\code{\link{coo.calliper}}, \code{\link{coo.calliper}};
\code{\link{coo.centdist}}, \code{\link{coo.centdist}};
\code{\link{coo.center}}, \code{\link{coo.center}};
\code{\link{coo.centpos}}, \code{\link{coo.centpos}};
\code{\link{coo.centsize}}, \code{\link{coo.centsize}};
\code{\link{coo.check}}; \code{\link{coo.close}},
\code{\link{coo.close}}; \code{\link{coo.down}};
\code{\link{coo.force2close}};
\code{\link{coo.interpolate}},
\code{\link{coo.interpolate}}; \code{\link{coo.ldk}},
\code{\link{coo.ldk}}; \code{\link{coo.perim.pts}},
\code{\link{coo.perim.pts}}; \code{\link{coo.perim}},
\code{\link{coo.perim}}; \code{\link{coo.rev}};
\code{\link{coo.rotatecenter}},
\code{\link{coo.rotatecenter}}; \code{\link{coo.rotate}},
\code{\link{coo.rotate}}; \code{\link{coo.samplerr}},
\code{\link{coo.samplerr}}; \code{\link{coo.scale}},
\code{\link{coo.scale}}; \code{\link{coo.slide}},
\code{\link{coo.slide}}; \code{\link{coo.smooth}},
\code{\link{coo.smooth}}; \code{\link{coo.trans}},
\code{\link{coo.trans}}; \code{\link{coo.up}};
\code{\link{is.closed}}, \code{\link{is.closed}}
}
\keyword{coo}

