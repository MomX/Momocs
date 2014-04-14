\name{coo.sample.int}
\alias{coo.sample.int}
\title{Interpolates coordinates}
\usage{
coo.sample.int(coo, n)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Out object.}
}
\value{
a matrix of (x,y) coordinates or an Out object.
}
\description{
Interpolates n coordinates "among existing points"between"
existing points, along the perimeter of the coordinates
provided and keeping the first point
}
\examples{
data(bot)
stack(bot)
stack(coo.sample.int(coo.sample(bot, 12), 120))
coo.plot(b[4])
coo.plot(coo.sample.int(coo.sample(b[4], 12), 120))
}
\seealso{
Other coo: \code{\link{coo.align}},
\code{\link{coo.align}}; \code{\link{coo.center}},
\code{\link{coo.center}}; \code{\link{coo.centpos}},
\code{\link{coo.centpos}}; \code{\link{coo.check}};
\code{\link{coo.close}}, \code{\link{coo.close}};
\code{\link{coo.rotate}}, \code{\link{coo.rotate}};
\code{\link{coo.sample}}, \code{\link{coo.sample.rr}};
\code{\link{coo.sample}}, \code{\link{coo.sample}};
\code{\link{coo.scale}}, \code{\link{coo.scale}};
\code{\link{coo.slide}}, \code{\link{coo.slide}};
\code{\link{coo.smooth}}, \code{\link{coo.smooth}};
\code{\link{coo.trans}}, \code{\link{coo.trans}};
\code{\link{is.closed}}, \code{\link{is.closed}}
}
\keyword{coo}

