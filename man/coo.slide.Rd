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
data(bot)
stack(bot)
stack(coo.slide(bot, 50))
coo.plot(bot[4])
coo.plot(coo.slide(coo.trans(bot[4], 40)))
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
\code{\link{coo.scale}}; \code{\link{coo.smooth}},
\code{\link{coo.smooth}}; \code{\link{coo.trans}},
\code{\link{coo.trans}}; \code{\link{is.closed}},
\code{\link{is.closed}}
}
\keyword{coo}

