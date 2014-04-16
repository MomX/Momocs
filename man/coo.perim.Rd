\name{coo.perim}
\alias{coo.perim}
\title{Calculates the perimeter}
\usage{
coo.perim(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates.}
}
\value{
a numeric, the perimeter.
}
\description{
Calculates the perimeter
}
\examples{
data(bot)
coo.perim(bot[4])
hist(sapply(bot$coo, coo.perim), breaks=10)
}
\keyword{coo_descriptors}

