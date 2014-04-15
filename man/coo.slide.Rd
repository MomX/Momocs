\name{coo.slide}
\alias{coo.slide}
\title{Slides coordinates}
\usage{
coo.slide(coo, id1)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Out object.}

  \item{id1}{the id of the point that will become the new
  first point.}
}
\value{
a matrix of (x,y) coordinates or an Out object.
}
\description{
Slides the coordinates so that the id1-th point become the
first one.
}
\examples{
data(hearts)
stack(hearts)
stack(coo.slide(hearts, 1))
coo.plot(hearts[4])
coo.plot(coo.slide(hearts[4], 50))
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
\code{\link{coo.samplerr}}; \code{\link{coo.sample}},
\code{\link{coo.sample}}; \code{\link{coo.scale}},
\code{\link{coo.scale}}; \code{\link{coo.smooth}},
\code{\link{coo.smooth}}; \code{\link{coo.trans}},
\code{\link{coo.trans}}; \code{\link{coo.up}};
\code{\link{is.closed}}, \code{\link{is.closed}}
}
\keyword{coo}

