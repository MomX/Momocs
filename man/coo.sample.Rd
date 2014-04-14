\name{coo.sample}
\alias{coo.sample}
\title{Sample coordinates (among points)}
\usage{
coo.sample(coo, n)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Out object.}
}
\value{
a matrix of (x,y) coordinates or an Out object.
}
\description{
Sample n coordinates among existing points
}
\examples{
data(bot)
stack(bot)
stack(coo.sample(bot, 12))
coo.plot(b[4])
coo.plot(coo.sample(b[4], 12))
}
\seealso{
Other coo: \code{\link{coo.align}},
\code{\link{coo.align}}; \code{\link{coo.center}},
\code{\link{coo.center}}; \code{\link{coo.centpos}},
\code{\link{coo.centpos}}; \code{\link{coo.check}};
\code{\link{coo.close}}, \code{\link{coo.close}};
\code{\link{coo.rotate}}, \code{\link{coo.rotate}};
\code{\link{coo.sample.int}}, \code{\link{coo.sample.int}};
\code{\link{coo.sample}}, \code{\link{coo.sample.rr}};
\code{\link{coo.scale}}, \code{\link{coo.scale}};
\code{\link{coo.slide}}, \code{\link{coo.slide}};
\code{\link{coo.smooth}}, \code{\link{coo.smooth}};
\code{\link{coo.trans}}, \code{\link{coo.trans}};
\code{\link{is.closed}}, \code{\link{is.closed}}
}
\keyword{coo}

