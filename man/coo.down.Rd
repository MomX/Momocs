\name{coo.down}
\alias{coo.down}
\title{Retains coordinates with negative y-coordinates}
\usage{
coo.down(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates.}
}
\value{
a matrix of (x,y) coordinates.
}
\description{
Useful when shapes are aligned along the x-axis (e.g.
because of a bilateral symmetry) and when one wants to
retain just the lower side.
}
\examples{
data(bot)
b <- coo.align.xax(bot[4])
coo.plot(b)
coo.plot(coo.down(b))
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
\code{\link{coo.close}}; \code{\link{coo.force2close}};
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

