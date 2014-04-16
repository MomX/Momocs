\name{coo.area}
\alias{coo.area}
\title{Calculates the area}
\usage{
coo.area(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates.}
}
\value{
a numeric, the area.
}
\description{
Calculates the area for any non-crossing polygon.
}
\examples{
data(bot)
coo.area(bot[4])
hist(sapply(bot$coo, coo.area), breaks=10)
}
\keyword{coo_descriptors}

