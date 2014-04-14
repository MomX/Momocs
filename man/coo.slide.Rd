\name{coo.slide}
\alias{coo.slide}
\title{Slides coordinates}
\usage{
coo.slide(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Out object.}
}
\value{
a matrix of (x,y) coordinates or an Out object.
}
\description{
Slides the coordinates so that the id1-th point become the
first one.
}
\examples{
data(bot)
stack(bot)
stack(coo.slide(bot, 50))
coo.plot(b[4])
coo.slide(coo.trans(b[4], 40))coo.slide <- function(coo, id1){UseMethod("coo.slide")}
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
\code{\link{coo.sample}}, \code{\link{coo.sample}};
\code{\link{coo.scale}}, \code{\link{coo.scale}};
\code{\link{coo.smooth}}, \code{\link{coo.smooth}};
\code{\link{coo.trans}}, \code{\link{coo.trans}};
\code{\link{is.closed}}, \code{\link{is.closed}}
}
\keyword{coo}

