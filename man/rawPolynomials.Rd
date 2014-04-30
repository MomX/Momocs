\name{rawPolynomials}
\alias{rawPolynomials}
\title{Calculates elliptical Fourier transforms on outlines}
\usage{
rawPolynomials(Opn, degree, baseline1, baseline2, nb.pts)
}
\arguments{
  \item{Opn}{an \link{Opn} object}

  \item{degree}{of the polynomial}

  \item{baseline1}{numeric the \eqn{(x; y)} coordinates of
  the first baseline by default \eqn{(x= -1; y=0)}}

  \item{baseline2}{numeric the \eqn{(x; y)} coordinates of
  the second baseline by default \eqn{(x= 1; y=0)}}

  \item{nb.pts}{number of points to sample and on which to
  calculate polynomials}
}
\value{
a \code{Coe} object.
}
\description{
Calculates elliptical Fourier transforms on outlines
}
\examples{
data(bot)
b <- bot[1]
}
\keyword{Opn}

