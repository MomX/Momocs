\name{Out}
\alias{Out}
\title{Builds an Out object}
\usage{
Out(coo.list, ldk = list(), fac = data.frame())
}
\arguments{
  \item{coo.list}{\code{list} of matrices of \eqn{(x; y)}
  coordinates}

  \item{ldk}{(optionnal) \code{list} of landmarks as row
  number indices}

  \item{fac}{(optionnal) a \code{data.frame} of factors,
  specifying the grouping structure}
}
\value{
an \code{Out} object
}
\description{
In Momocs, \code{Out} classes objects are lists of
\bold{closed} outlines, on which generic methods such as
plotting methods (e.g. \link{stack}) and specific methods
(e.g. \link{eFourier} can be applied.  \code{Out} objects
are primarily \code{\link{Coo}} objects.
}
\details{
These methods can be applied on \code{Out} objects:
\enumerate{ \item Handling: \code{subset, coo.} family;
\item Graphics: \code{plot, stack, panel}; \item
Calibration : \code{hqual, hquant, hpow}; \item Morpho:
\code{eFourier, tFourier, tFourier, Ptolemy}. }
}
\seealso{
\link{Coo}, \link{Opn}
}
\keyword{Out}

