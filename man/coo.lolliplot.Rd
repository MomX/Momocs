\name{coo.lolliplot}
\alias{coo.lolliplot}
\title{Plots differences between two configuratins}
\usage{
coo.lolliplot(coo1, coo2, type = c("lolli", "arrow")[1])
}
\arguments{
  \item{coo1}{A \code{list} or a \code{matrix} of
  coordinates.}

  \item{coo2}{A \code{list} or a \code{matrix} of
  coordinates.}

  \item{type}{either "lolli" or "arrow" to draw segments or
  arrows between pairs of points.}

  \item{...}{optional parameters for coo.plot}
}
\description{
Draws "lollipops" between two configurations
}
\examples{
data(bot)
b1 <- coo.center(coo.sample(bot[4], 24))
b2 <- b1*1.2
coo.lolliplot(b1, b2)
}
\keyword{graphics}

