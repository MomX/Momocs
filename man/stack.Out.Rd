\name{stack.Out}
\alias{stack.Out}
\title{Plot on Out objects: stacks all shapes}
\usage{
\method{stack.Out}{}(x, cols, borders, points = FALSE, first.point = TRUE,
  centroid = TRUE, ldk = TRUE, ldk.pch = 3, ldk.col = "#FF000055",
  ldk.cex = 0.5, xy.axis = TRUE, ...)
}
\arguments{
  \item{x}{The \code{Out} object to plot.}

  \item{cols}{A \code{vector} of colors for drawing the
  outlines. Either a single value or of length exactly
  equals to the number of coordinates.}

  \item{borders}{A \code{vector} of colors for drawing the
  borders. Either a single value or of length exactly
  equals to the number of coordinates.}

  \item{points}{logical whether to draw or not points}

  \item{first.point}{logical whether to draw or not the
  first point}

  \item{centroid}{logical whether to draw or not the
  centroid}

  \item{ldk}{\code{logical}. Whether to display landmarks
  (if any).}

  \item{ldk.pch}{A \code{pch} for these landmarks.}

  \item{ldk.col}{A color for these landmarks.}

  \item{ldk.cex}{A \code{cex} fro these landmarks}

  \item{xy.axis}{whether to draw or not the x and y axes}

  \item{...}{further arguments to be passed to coo.plot}
}
\description{
Plots all the outlines from a \code{Out} on the same graph
with graphical options.
}
\examples{
data(mosquito)
stack(mosquito, borders="#1A1A1A22", first.point=FALSE)
data(hearts)
stack(hearts)
stack(hearts, ldk=FALSE)
stack(hearts, borders="#1A1A1A22", ldk=TRUE, ldk.col=col.summer(4), ldk.pch=20)
}
\seealso{
\link{panel.Out}, \link{plot.Out}.
}

