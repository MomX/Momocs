\name{OpnCoe}
\alias{OpnCoe}
\title{Builds an OpnCoe object}
\usage{
OpnCoe(coe = matrix(), fac = data.frame(), method = character(),
  baseline1 = numeric(), baseline2 = numeric(), mod = list())
}
\arguments{
  \item{coe}{\code{matrix} of morphometric coefficients}

  \item{fac}{(optionnal) a \code{data.frame} of factors,
  specifying the grouping structure}

  \item{method}{used to obtain these coefficients}

  \item{baseline1}{\eqn{(x; y)} coordinates of the first
  baseline point}

  \item{baseline2}{\eqn{(x; y)} coordinates of the second
  baseline point}

  \item{mod}{an R \link{lm} object, used to reconstruct
  shapes}
}
\value{
an \code{OpnCoe} object
}
\description{
In Momocs, \code{OpnCoe} classes objects are wrapping
around lists of morphometric coefficients, along with other
informations, on which generic methods such as plotting
methods (e.g. \link{boxplot}) and specific methods can be
applied.  \code{OpnCoe} objects are primarily
\code{\link{Coe}} objects.
}
\details{
These methods can be applied on \code{Opn} objects:
\enumerate{ \item Graphics: \code{hist, boxplot}; \item
Multivariate analyses: \code{pca, etc.}. }
}
\seealso{
\link{Coe}, \link{OutCoe}
}
\keyword{OpnCoe}

