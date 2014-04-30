\name{Opn}
\alias{Opn}
\title{Builds an Opn object}
\usage{
Opn(coo.list, ldk = list(), fac = data.frame())
}
\arguments{
  \item{coo}{a list of matrices of \eqn{(x; y)}
  coordinates.}

  \item{ldk}{(optionnal) a list of landmarks on these
  coordinates. Not yet used for Opn objects but might be in
  a near future. See \link{Coo} methods.  (provided as the
  row numbers) for every outline}

  \item{fac}{(optionnal) a \code{data.frame} of factors,
  specifying the grouping structure.}
}
\value{
an \code{Out} object.
}
\description{
In Momocs, \code{Opn} classes objects are lists of
\bold{open} outlines, on which generic methods such as
plotting methods (e.g. \link{stack}) and specific methods
(e.g. \link{rawPolynomials} can be applied.
}
\details{
\code{Opn} objects are primarily \link{\code{Coo}} objects.

\code{Opn} objects are lists, that consist of the following
slots: \describe{ \item{$coo}{the coordinates}
\item{$ldk}{landmarks} \item{$fac}{some classifiers} }

These methods can be applied on \code{Opn} objects:
\enumerate{ \item Handling: subset, all the coo.xxx family;
\item Graphics: plot, stack, panel; \item Morpho:
rawPolynomials, orthoPolynomials. }
}
\seealso{
Other res: \code{\link{Out}}
}
\keyword{Opn}

