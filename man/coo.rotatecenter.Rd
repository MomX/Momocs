\name{coo.rotatecenter}
\alias{coo.rotatecenter}
\title{Rotates shapes with a custom center}
\usage{
coo.rotatecenter(coo, theta, center = c(0, 0))
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Out object.}

  \item{theta}{an angle, in radians.}

  \item{center}{the (x, y) position of the center}
}
\value{
a matrix of (x,y) coordinates or an Out object.
}
\description{
rotates a shape of "theta" angles (in radians) and with a
(x, y) "center".
}
\examples{
data(bot)
coo.plot(bot[4])
coo.plot(coo.rotatecenter(bot[4], pi/2, c(0, 0)))
}
\seealso{
Other coo: \code{\link{coo.align}},
\code{\link{coo.align}}; \code{\link{coo.bookstein}},
\code{\link{coo.bookstein}}; \code{\link{coo.center}},
\code{\link{coo.center}}; \code{\link{coo.centpos}},
\code{\link{coo.centpos}}; \code{\link{coo.check}};
\code{\link{coo.close}}, \code{\link{coo.close}};
\code{\link{coo.perim.pts}}, \code{\link{coo.perim.pts}};
\code{\link{coo.rotate}}, \code{\link{coo.rotate}};
\code{\link{coo.sample.int}}, \code{\link{coo.sample.int}};
\code{\link{coo.sample.rr}}, \code{\link{coo.sample.rr}};
\code{\link{coo.sample}}, \code{\link{coo.sample}};
\code{\link{coo.scale}}, \code{\link{coo.scale}};
\code{\link{coo.slide}}, \code{\link{coo.slide}};
\code{\link{coo.smooth}}, \code{\link{coo.smooth}};
\code{\link{coo.trans}}, \code{\link{coo.trans}};
\code{\link{is.closed}}, \code{\link{is.closed}}
}
\keyword{coo}

