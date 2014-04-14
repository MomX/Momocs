\name{coo.close}
\alias{coo.close}
\title{Closes/'Uncloses' shapes}
\usage{
coo.close(coo)
}
\arguments{
  \item{coo}{a matrix of (x,y) coordinates or a Out object}
}
\value{
a matrix of (x,y) coordinates or an Out object.
}
\description{
Returns a (un)closed shape from (un)closed shapes.
}
\examples{
x <- (matrix(1:10, ncol=2))
x2 <- coo.close(x)
x3 <- coo.unclose(x2)
x
is.closed(x)
x2
is.closed(x2)
x3
is.closed(x3)
}
\seealso{
Other coo: \code{\link{coo.align}},
\code{\link{coo.align}}; \code{\link{coo.bookstein}},
\code{\link{coo.bookstein}}; \code{\link{coo.center}},
\code{\link{coo.center}}; \code{\link{coo.centpos}},
\code{\link{coo.centpos}}; \code{\link{coo.check}};
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

