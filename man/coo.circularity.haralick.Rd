\name{coo.circularity.haralick}
\alias{coo.circularity.haralick}
\title{Calculates the Haralick's circularity of a shape}
\usage{
coo.circularity.haralick(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates.}
}
\value{
a numeric, the Haralick's circularity.
}
\description{
Returns Haralick's circularity which is less sensible to
digitalization noise than coo.circularity. See ...#todo
}
\examples{
data(bot)
coo.circularity.haralick(bot[4])
}
\keyword{coo_descriptors}

