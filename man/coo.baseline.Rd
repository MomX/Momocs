\name{coo.baseline}
\alias{coo.baseline}
\title{Register new baselines}
\usage{
coo.baseline(coo, ldk1, ldk2, t1, t2)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Out object.}

  \item{ldk1}{the id of the first point of the new
  baseline}

  \item{ldk2}{the id of the second point of the new
  baseline}

  \item{t1}{a numerix for the (x,y) coordinates of the
  first point of the new baseline}

  \item{t2}{a numerix for the (x,y) coordinates of the
  second point of the new baseline}
}
\value{
a matrix of (x,y) coordinates or an Out object.
}
\description{
A non-exact baseline registration on t1 and t2 coordinates,
}
\examples{
data(hearts)
stack(hearts)
# note the scale
stack(coo.baseline(hearts, 2, 4, c(-1, 0), c(1, 0)))
}
\seealso{
Other coo: \code{\link{coo.align.xax}};
\code{\link{coo.align}}, \code{\link{coo.align}};
\code{\link{coo.area}}, \code{\link{coo.area}};
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
\code{\link{coo.samplerr}}; \code{\link{coo.sample}},
\code{\link{coo.sample}}; \code{\link{coo.scale}},
\code{\link{coo.scale}}; \code{\link{coo.slide}},
\code{\link{coo.slide}}; \code{\link{coo.smooth}},
\code{\link{coo.smooth}}; \code{\link{coo.trans}},
\code{\link{coo.trans}}; \code{\link{coo.up}};
\code{\link{is.closed}}, \code{\link{is.closed}}
}
\keyword{coo}

