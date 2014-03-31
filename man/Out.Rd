\name{Out}
\alias{Coo}
\alias{Out}
\title{Builds an Out object}
\usage{
Out(coo.list, ldk = list(), fac = data.frame())
}
\arguments{
  \item{coo}{a matrix of (x,y) coordinates or a \code{Out}
  object.}

  \item{ldk}{(optionnal) a list of landmarks on these
  coordinates (provided as the row numbers) for every
  outline}

  \item{fac}{(optionnal) a data.frame of factors,
  specifying the grouping structure.}
}
\value{
a \code{Out} object.
}
\description{
In Momocs, Out classes objects are lists of closed
outlines, on which generic methods such as plotting methods
(e.g. stack()) and specific methods (e.g. efourier()) can
be applied.
}
\details{
They must be built from a list (or an array) of
coordinates.
}
\examples{
coo.list <- list(out1=matrix(1:10, nc=2),
out2=matrix(1:20, nc=2))
anOutobject <- Out(coo.list)
anOutobject
}
\keyword{Out}

