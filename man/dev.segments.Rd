\name{dev.segments}
\alias{dev.segments}
\title{Draws colored segments from a matrix of coordinates.}
\usage{
dev.segments(coo, cols, lwd = 1)
}
\arguments{
  \item{coo}{A matrix of coordinates.}

  \item{cols}{A vector of color of \code{length =
  nrow(coo)}.}

  \item{lwd}{The \code{lwd} to use for drawing segments.}
}
\description{
Given a matrix of (x; y) coordinates, draws segments
between every points defined by the row of the matrix and
uses a color to display an information.
}
\examples{
# we load some data
data(bot)
guinness <- bot[9]

# we calculate the best possible outline and one with 12 harm.
out.best <- l2m(efourier.i(efourier(guinness, nb.h=-1), nb.pts=120))
out.12   <- l2m(efourier.i(efourier(guinness, nb.h=12), nb.pts=120))

# we calculate deviations, you can also try 'edm'
dev <- edm.nearest(out.12, out.best) / coo.centsize(out.12)

# we prepare the color scale
d.cut <- cut(dev, breaks=20, labels=FALSE, include.lowest=TRUE)
cols  <- paste0(col.summer(20)[d.cut], "CC")

# we draw the results
coo.plot(out.best, border="black", col="grey80", main="Guiness fitted by 20 harm.")
dev.segments(out.12, cols=cols, lwd=4)
}
\keyword{graphics}

