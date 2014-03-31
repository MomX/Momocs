\name{coo.trans}
\alias{coo.trans}
\title{Translates coordinates}
\usage{
coo.trans(coo, x = 0, y = 0)
}
\arguments{
  \item{coo}{a matrix of (x,y) coordinates or a \code{Out}
  object.}
}
\value{
a matrix of (x,y) coordinates or a \code{Out} object.
}
\description{
Translates the coordinatesby a 'x' and 'y' value
}
\examples{
coo.trans(matrix(1:10, ncol=2), 5, 10)
}
\seealso{
Other coo: \code{\link{coo.align}},
\code{\link{coo.align}}; \code{\link{coo.center}},
\code{\link{coo.center}}; \code{\link{coo.check}};
\code{\link{coo.rotate}}, \code{\link{coo.rotate}};
\code{\link{coo.scale}}, \code{\link{coo.scale}};
\code{\link{is.closed}}, \code{\link{is.closed}}
}
\keyword{coo}

