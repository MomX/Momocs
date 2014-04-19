\name{tps.arr}
\alias{tps.arr}
\title{Deformation "vector field" using Thin Plate Splines.}
\usage{
tps.arr(fr, to, amp=1, palette = col.summer, arr.nb = 100, arr.levels
= 100, arr.len = 0.1, arr.ang = 30, arr.lwd = 1, arr.col = "grey50", shp =
TRUE, shp.col = rep(NA, 2), shp.border=col.gallus(2), shp.lwd = c(2, 2),
shp.lty = c(1, 1))
}
\arguments{
  \item{fr}{The reference \eqn{(x; y)} coordinates.}

  \item{to}{The target \eqn{(x; y)} coordinates.}

  \item{amp}{An amplification factor of differences between
  \code{fr} and \code{to}.}

  \item{palette}{A color palette such those included in
  Momocs or produced with \link{colorRampPalette}.}

  \item{arr.nb}{A \code{numeric}. The number of arrows to
  calculate.}

  \item{arr.levels}{A \code{numeric}. The number of levels
  for the color of arrows.}

  \item{arr.len}{A \code{numeric}. The length of arrows.}

  \item{arr.ang}{A \code{numeric}. The angle for arrows'
  heads.}

  \item{arr.lwd}{A \code{numeric}. The \code{lwd} for
  drawing arrows.}

  \item{arr.col}{If \code{palette} is not used the color
  for arrwos.}

  \item{shp}{\code{logical}. Whether to draw shapes.}

  \item{shp.col}{Two colors for filling the shapes.}

  \item{shp.border}{Two colors for drawing the borders.}

  \item{shp.lwd}{Two \code{lwd} for drawing shapes.}

  \item{shp.lty}{Two \code{lty} fro drawing the shapes.}
}
\value{
No returned value.
}
\description{
\code{tps.arr}(ows) calculates deformations between two
configurations and illustrate them using arrows.
}
\examples{
data(bot)
botF <- eFourier(bot)
x <- meanShapes(botF, "type", nb.pts=80)
fr <- x$beer
to <- x$whisky
tps.arr(fr, to, arr.nb=400, palette=col.sari, amp=3)
}
\keyword{Utilities}
\keyword{coo}

