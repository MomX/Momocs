\name{is.closed}
\alias{is.closed}
\title{Tests if shapes are closed}
\usage{
is.closed(coo)
}
\arguments{
  \item{coo}{a matrix of (x,y) coordinates or a Out object}
}
\value{
a boolean.
}
\description{
Returns TRUE/FALSE whether the last coordianate of the
shapes is the same as the first one.
}
\examples{
is.closed(matrix(1:10, ncol=2))
}
\seealso{
Other coo: \code{\link{coo.align}},
\code{\link{coo.align}}; \code{\link{coo.center}},
\code{\link{coo.center}}; \code{\link{coo.check}};
\code{\link{coo.rotate}}, \code{\link{coo.rotate}};
\code{\link{coo.scale}}, \code{\link{coo.scale}};
\code{\link{coo.trans}}, \code{\link{coo.trans}}
}
\keyword{coo}

