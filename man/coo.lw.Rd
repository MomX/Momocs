\name{coo.lw}
\alias{coo.lw}
\title{Calculates length and width}
\usage{
coo.lw(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates.}
}
\value{
two numeric, the length and the width.
}
\description{
Returns the length and width of a shape based on their
iniertia axis i.e. alignment to the x-axis. The length is
defined as the range along the x-axis; the width as the
range on the y-axis.
}
\examples{
data(bot)
coo.area(bot[4])
}
\keyword{coo_descriptors}

