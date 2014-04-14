\name{coo.trans}
\alias{coo.trans}
\title{Translates coordinates}
\usage{
coo.trans(coo, x = 0, y = 0)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Out object.}

  \item{x}{the translation on the x dimension.}

  \item{y}{the translation on the y dimension.}
}
\value{
a matrix of (x,y) coordinates or an Out object.
}
\description{
Translates the coordinatesby a 'x' and 'y' value
}
\examples{
data(bot)
stack(bot)
stack(coo.trans(bot, 10, 10))
coo.plot(bot[4])
coo.plot(coo.trans(bot[4], 10, 10))
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
\code{\link{coo.scale}}; \code{\link{coo.slide}},
\code{\link{coo.slide}}; \code{\link{coo.smooth}},
\code{\link{coo.smooth}}; \code{\link{is.closed}},
\code{\link{is.closed}}
}
\keyword{coo}

