\name{coo.force2close}
\alias{coo.force2close}
\title{Forces shapes to close}
\usage{
coo.force2close(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates.}
}
\value{
a matrix of (x,y) coordinates.
}
\description{
An exotic function that distribute the distance between the
first and the last points of unclosed shapes, so that they
become closed. May be useful (?) e.g. for t/rfourier
methods where reconstructed shapes may not be closed.
}
\examples{
data(bot)
b <- coo.sample(bot[4], 64)
b <- b[1:40,]
coo.plot(b)
coo.plot(coo.force2close(b))
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
\code{\link{coo.interpolate}},
\code{\link{coo.interpolate}}; \code{\link{coo.ldk}},
\code{\link{coo.ldk}}; \code{\link{coo.perim.pts}},
\code{\link{coo.perim.pts}}; \code{\link{coo.perim}},
\code{\link{coo.perim}}; \code{\link{coo.rev}};
\code{\link{coo.rotatecenter}},
\code{\link{coo.rotatecenter}}; \code{\link{coo.rotate}},
\code{\link{coo.rotate}}; \code{\link{coo.samplerr}},
\code{\link{coo.samplerr}}; \code{\link{coo.sample}},
\code{\link{coo.sample}}; \code{\link{coo.scale}},
\code{\link{coo.scale}}; \code{\link{coo.slide}},
\code{\link{coo.slide}}; \code{\link{coo.smooth}},
\code{\link{coo.smooth}}; \code{\link{coo.trans}},
\code{\link{coo.trans}}; \code{\link{coo.up}};
\code{\link{is.closed}}, \code{\link{is.closed}}
}
\keyword{coo}

