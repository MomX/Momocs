\name{coo.eccentricity.boundingbox}
\alias{coo.eccentricity.boundingbox}
\title{Calculates the eccentricity (bounding box) of a shape}
\usage{
coo.eccentricity.boundingbox(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates.}
}
\value{
a numeric, the eccentricity (boundingbox)
}
\description{
Calculated using the width / length ratio. See
\link{coo.lw}
}
\examples{
data(bot)
coo.eccentricity.boundingbox(bot[4])
}
\keyword{coo_descriptors}

