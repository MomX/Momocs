\name{mshapes}
\alias{mshapes}
\title{Mean shape calculation from matrices of coefficients}
\usage{
mshapes(Coe, fac, nb.pts)
}
\arguments{
  \item{Coe}{a \link{Coe} object}

  \item{fac}{factor from the $fac slot. See examples
  below.}

  \item{nb.pts}{numeric the number of points for calculated
  shapes}
}
\value{
a list of matrices of (x,y) coordinates.
}
\description{
Calculates mean shapes on matrices of coefficients by
groups (if passed with a "fac") or globally (if not).
}
\keyword{multivariate}

