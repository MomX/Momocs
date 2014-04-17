\name{polynomials}
\alias{polynomials}
\title{Calculate polynomial fit on open outlines}
\usage{
polynomials(coo, n, orthogonal = TRUE)
}
\arguments{
  \item{coo}{a matrix or a list of (x; y) coefficients}

  \item{n}{the degree of the polynomial to fit (the
  intercept is returned)}

  \item{orthogonal}{logical wheter to calculate orthogonal
  coefficients}
}
\value{
a lm model object
}
\description{
Returns a model obtained with a lm with polynomial
coefficients, orthogonal or not, from a matrix of (x; y)
coordinates (assumed to be normalized).
}
\keyword{morphoCore}

