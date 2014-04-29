\name{mshapes}
\alias{mshapes}
\title{Mean shapes}
\usage{
mshapes(Coe, fac, nb.pts)
}
\arguments{
  \item{Coe}{a Coe object}

  \item{fac}{factor from the data.frame in Coe$fac, if
  empty returns global mshape}

  \item{nb.pts}{numeric the number of points for calculated
  shapes}
}
\value{
a list of matrices of (x,y) coordinates.
}
\description{
Calculates mean shapes on matrices of coefficients by
groups or globally.
}
\keyword{multivariate}

