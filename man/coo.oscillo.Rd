\name{coo.oscillo}
\alias{coo.oscillo}
\title{Momocs' "oscilloscope" for periodic functions.}
\usage{
coo.oscillo(coo, rug = TRUE, legend = TRUE,
cols = col.gallus(2), nb.pts=12)
}
\arguments{
  \item{coo}{A list or a matrix of coordinates.}

  \item{rug}{\code{logical}. Whether to display a pseudo
  rug, that indicate if the derivate is positive.}

  \item{legend}{\code{logical}. Whether to add a legend.}

  \item{cols}{A \code{vector} of two colors for lines.}

  \item{nb.pts}{\code{integer}. The number or reference
  points, sampled equidistantly along the curvilinear
  abscissa and added on the oscillo curves.}
}
\description{
Shape analysis deals with curve fitting, whether \eqn{x(t)}
and \eqn{y(t)} positions along the curvilinear abscissa or
radius/tangent angle variation. We may need to represent
these single or double periodic functions that are ajusted
by Fourier-based method. \code{coo.oscillo} and
\code{coo.oscillo1} compute and provide standardized plot
when given a matrix of coordinates or a vector,
respectively. These functions are mainly used for
development purpose but are included in the package.
}
\examples{
data(bot)
coo.oscillo(bot[1])
}
\keyword{Utilities}

