\name{hist.OutCoe}
\alias{hist.OutCoe}
\title{Histogram on OutCoe matrices of harmonic coefficients}
\usage{
\method{hist}{OutCoe}(x, retain, drop, palette = col.gallus,
  title = "Variation of harmonic coefficients", ...)
}
\arguments{
  \item{x}{the \link{OutCoe} object}

  \item{retain}{numeric the number of harmonics to retain}

  \item{drop}{numeric the number of harmonics to drop}

  \item{palette}{a color \link{palette}}

  \item{title}{a title for the plot}

  \item{...}{useless here but maintain the consistency with
  generic hist}
}
\description{
Explores the distribution of harmonic coefficient values
}
\examples{
data(bot)
bot.f <- eFourier(bot, 24)
hist(bot.f)
}
\seealso{
\link{boxplot.OutCoe}
}
\keyword{OutCoe}
\keyword{graphics}

