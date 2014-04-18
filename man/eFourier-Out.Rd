\name{eFourier}
\alias{eFourier}
\title{Calculates elliptical Fourier transforms on Out objects}
\usage{
eFourier(Out, nb.h, smooth.it, norm, start)
}
\arguments{
  \item{Out}{the Out object on which to calculate eft}

  \item{nb.h}{the number of harmonics to calculate}

  \item{smooth.it}{the number of smoothing iterations to
  perform}

  \item{norm}{whether to normalize the matrix of
  coefficients. See Details.}

  \item{start}{logical whether to consider the first point
  as homologous}
}
\description{
A wrapper for \link{efourier} to be applied on Out objects.
}
\examples{
data(bot)
eFourier(bot, 12)
}

