\name{tps.iso}
\alias{tps.iso}
\title{Deformation isolines using Thin Plate Splines.}
\usage{
tps.iso(fr, to, amp=1, palette = col.summer, iso.nb = 500, iso.levels
= 12, cont=TRUE, cont.col="black", shp = TRUE, shp.col = rep(NA, 2),
shp.border=col.gallus(2), shp.lwd = c(2, 2), shp.lty = c(1, 1))
}
\arguments{
  \item{fr}{The reference \eqn{(x; y)} coordinates.}

  \item{to}{The target \eqn{(x; y)} coordinates.}

  \item{amp}{An amplification factor of differences between
  \code{fr} and \code{to}.}

  \item{palette}{A color palette such those included in
  Momocs or produced with \link{colorRampPalette}.}

  \item{iso.levels}{\code{numeric}. The number of levels
  for mapping the deformations.}

  \item{iso.nb}{A \code{numeric}. The number of points to
  use for the calculation of deformation.}

  \item{cont}{\code{logical}. Whether to draw contour
  lines.}

  \item{cont.col}{A color for drawing the contour lines.}

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
\code{tps.iso} calculates deformations between two
configurations and map them with or without isolines.
}
\examples{
data(bot)
botF <- eFourier(bot)
x <- mshapes(botF, "type", nb.pts=80)
fr <- x$beer
to <- x$whisky
tps.iso(fr, to, iso.nb=2000, amp=3)
}
\keyword{Utilities}
\keyword{coo}

