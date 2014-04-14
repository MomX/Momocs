\name{coo.align}
\alias{coo.align}
\title{Aligns coordinates}
\usage{
coo.align(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Out object.}
}
\value{
a matrix of (x,y) coordinates or an Out object.
}
\description{
Aligns the coordinates along their longer axis using
var-cov matrix and eigen values.
}
\examples{
data(bot)
stack(bot)
stack(coo.align(bot))
coo.plot(bot[4])
coo.plot(coo.align(bot[4]))
}
\seealso{
Other coo: \code{\link{coo.bookstein}},
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
\code{\link{coo.slide}}; \code{\link{coo.smooth}},
\code{\link{coo.smooth}}; \code{\link{coo.trans}},
\code{\link{coo.trans}}; \code{\link{is.closed}},
\code{\link{is.closed}}
}
\keyword{coo}

