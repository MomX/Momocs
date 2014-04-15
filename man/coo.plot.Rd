\name{coo.plot}
\alias{coo.plot}
\title{Plots a single outline.}
\usage{
coo.plot(coo, xlim, ylim, border = "#333333", col = "#33333322", lwd = 1,
  lty = 1, points = FALSE, first.point = TRUE, centroid = TRUE,
  xy.axis = TRUE, pch = 1, cex = 0.5, main, plot.new = TRUE)
}
\arguments{
  \item{coo}{A \code{list} or a \code{matrix} of
  coordinates.}

  \item{xlim}{If \code{coo.plot} is called and \code{coo}
  is missing, then a vector of length 2 specifying the
  \code{ylim} of the ploting area.}

  \item{ylim}{If \code{coo.plot} is called and \code{coo}
  is missing, then a vector of length 2 specifying the
  \code{ylim} of the ploting area.}

  \item{border}{A color for the shape border.}

  \item{col}{A color to fill the shape polygon.}

  \item{lwd}{The \code{lwd} for drawing shapes.}

  \item{lty}{The \code{lty} for drawing shapes.}

  \item{points}{\code{logical}. Whether to display points.
  If missing and number of points is < 100, then points are
  plotted.}

  \item{first.point}{\code{logical} whether to plot or not
  the first point.}

  \item{centroid}{\code{logical}. Whether to display
  centroid.}

  \item{xy.axis}{\code{logical}. Whether to draw the xy
  axis.}

  \item{pch}{The \code{pch} for points.}

  \item{cex}{The \code{cex} for points.}

  \item{main}{\code{character}. A title for the plot.}

  \item{plot.new}{\code{logical} whether to plot or not a
  new frame.}
}
\value{
No returned value.
}
\description{
A simple wrapper for plotting shapes. Widely used in
Momocs.
}
\examples{
data(bot)
coo.plot(bot[1])
}
\seealso{
coo.draw.
}

