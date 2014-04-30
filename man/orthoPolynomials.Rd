\name{orthoPolynomials}
\alias{orthoPolynomials}
\title{Calculates orthogonal polynomials on Opn}
\usage{
orthoPolynomials(Opn, degree, baseline1, baseline2, nb.pts)
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
a \code{OpnCoe} object.
}
\description{
Calculates orthogonal polynomials on Opn
}
\examples{
data(olea)
orthoPolynomials(olea, 5)
}
\keyword{Opn}

