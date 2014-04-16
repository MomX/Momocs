\name{coo.elongation}
\alias{coo.elongation}
\title{Calculates the elongation of a shape}
\usage{
coo.elongation(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates.}
}
\value{
a numeric, the circularity normalized to the unit circle.
}
\description{
Calculates the elongation of a shape
}
\examples{
data(bot)
coo.elongation(bot[4])
}
\keyword{coo_descriptors}

