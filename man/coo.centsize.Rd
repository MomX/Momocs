\name{coo.centsize}
\alias{coo.centsize}
\title{Calculates the centroid size}
\usage{
coo.centsize(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates.}
}
\value{
a numeric, the centroid size.
}
\description{
Calculates the centroid size
}
\examples{
data(bot)
coo.centsize(bot[4])
hist(sapply(bot$coo, coo.centsize), breaks=10)
}
\keyword{coo_descriptors}

