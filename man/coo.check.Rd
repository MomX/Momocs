\name{coo.check}
\alias{coo.check}
\title{Checks shape}
\usage{
coo.check(coo)
}
\arguments{
  \item{coo}{a matrix of (x,y) coordinates or a list.}
}
\value{
a matrix of (x,y) coordinates.
}
\description{
A simple utility, used internally, mostly in the coo
functions and methods. Returns a matrix, when passed with
either a list or a matrix of coordinates.
}
\examples{
#coo.check("Not a shape")
#coo.check(matrix(1:10, ncol=2))
#coo.check(list(x=1:5, y=6:10))
}
\seealso{
Other coo: \code{\link{coo.align}},
\code{\link{coo.align}}; \code{\link{coo.bookstein}},
\code{\link{coo.bookstein}}; \code{\link{coo.center}},
\code{\link{coo.center}}; \code{\link{coo.centpos}},
\code{\link{coo.centpos}}; \code{\link{coo.close}},
\code{\link{coo.close}}; \code{\link{coo.perim.pts}},
\code{\link{coo.perim.pts}};
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

