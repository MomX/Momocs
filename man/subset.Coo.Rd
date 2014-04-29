\name{subset.Coo}
\alias{subset}
\alias{subset.Coo}
\title{Create subsets of Coo objects}
\usage{
\method{subset}{Coo}(x, subset, ...)
}
\arguments{
  \item{x}{a \code{Coo} object, i.e. \link{Out} or
  \link{Opn}}

  \item{subset}{logical taken from the $fac slot, or
  indices. See examples.}

  \item{...}{useless here but maintains consistence with
  the generic subset.}
}
\description{
Pretty useful in morphometrics. Imagine you have a
\link{Out} or a \link{Opn} object that combines several
different \emph{groups}, whatever \emph{groups} are :
species, views, etc. You may be interested in doing
separated analyses (even if you could combine them later),
then this function will ease the process. See the examples
below.
}
\examples{
data(bot)
bot$fac
beers <- subset(bot, type=="beer")
whisk <- subset(bot, type=="whisky")
# or you may prefere indices
subset(bot, c(1, 13, 34, 37))
# and you can combine them :
data(olea)
olea$fac
subset(olea, domes=="cult" & view=="VL")
}
\keyword{Coo}
\keyword{Opn}

