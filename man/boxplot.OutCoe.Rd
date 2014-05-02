\name{boxplot.OutCoe}
\alias{boxplot.OutCoe}
\title{Boxplot on OutCoe matrices of harmonic coefficients}
\usage{
\method{boxplot}{OutCoe}(x, retain, drop, palette = col.gallus,
  title = "Variation of harmonic coefficients", legend = TRUE, ...)
}
\arguments{
  \item{x}{the \link{OutCoe} object}

  \item{retain}{numeric the number of harmonics to retain}

  \item{drop}{numeric the number of harmonics to drop}

  \item{palette}{a color \link{palette}}

  \item{title}{a title for the plot}

  \item{legend}{logical whether to add a legend}

  \item{...}{useless here but maintain the consistency with
  generic boxplot}
}
\description{
Allows to explore diversity of coefficients from OutCoe
objects, typically obtain after a eFourier, tFourier,
rFourier on an Out object.
}
\examples{
data(bot)
bot.f <- eFourier(bot, 24)
boxplot(bot.f)
}
\seealso{
\link{hist.OutCoe}
}
\keyword{OutCoe}
\keyword{graphics}

