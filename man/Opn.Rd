\name{Opn}
\alias{Opn}
\title{Builds an Opn object}
\usage{
Opn(coo.list, ldk = list(), fac = data.frame())
}
\arguments{
  \item{coo.list}{a list of (x; y) matrices of
  coordinates.}

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
In Momocs, Opn classes objects are lists of \bold{open}
outlines, on which generic methods such as plotting methods
(e.g. stack()) and specific methods (e.g. polynomials())
can be applied.
}
\details{
They must be built from a list (or an array) of (x; y)
coordinates matrices.
}
\seealso{
Other res: \code{\link{Coo}}, \code{\link{Out}};
\code{\link{coo.trans}}, \code{\link{coo.trans}}
}
\keyword{Opn}

