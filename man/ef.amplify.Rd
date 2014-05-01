\name{ef.amplify}
\alias{ef.amplify}
\title{Dilates shapes based on elliptical Fourier decomposition.}
\usage{
ef.amplify(ef, amp = rep(0.5, 4))
}
\arguments{
  \item{ef}{\code{list}. A list containing \eqn{a_n},
  \eqn{b_n}, \eqn{c_n} and \eqn{d_n} Fourier coefficients,
  such as returned by \code{efourier}.}

  \item{amp}{A vector of \code{numeric}. If \code{amp} is
  of length 4, the value specify the multiplication factor
  for \eqn{a_1}, \eqn{b_1}, \eqn{c_1} and \eqn{d_1} ; if
  only one value is provided, then the multiplication
  factor will be the same for the four coefficients
  \eqn{abcd_1}.}
}
\value{
\code{ef.amplify} returns the \code{ef} provided but with
"amplified" coefficients for the first harmonics.
}
\description{
Calculates dilated and eroded shapes based on elliptical
Fourier decomposition \emph{i.e.} taking into account the
shape as a whole. Lists created by \code{efourier} objects
can be passed to \code{ef.amplify}
}
\seealso{
\link{efourier} for a description of the elliptical Fourier
analysis and \link{Ptolemy} for an illustration of the
first ellipse/harmonic defining the shape "amplitude".
}
\keyword{coreMorpho}

