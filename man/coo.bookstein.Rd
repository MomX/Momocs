\name{coo.bookstein}
\alias{coo.bookstein}
\title{Register new baselines}
\usage{
coo.bookstein(coo, ldk1, ldk2)
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
coo.bookstein registers a new baseline for the shape, with
the ldk1-th
}
\examples{
data(hearts)
stack(hearts)
stack(coo.bookstein(hearts, 2, 4))
# note the scale
stack(coo.baseline(hearts, 2, 4, c(-1, 0), c(1, 0)))
}
\seealso{
Other coo: \code{\link{coo.align}},
\code{\link{coo.align}}; \code{\link{coo.center}},
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
\code{\link{coo.slide}}; \code{\link{coo.smooth}},
\code{\link{coo.smooth}}; \code{\link{coo.trans}},
\code{\link{coo.trans}}; \code{\link{is.closed}},
\code{\link{is.closed}}
}
\keyword{coo}

