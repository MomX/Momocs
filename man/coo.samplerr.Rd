\name{coo.samplerr}
\alias{coo.samplerr}
\title{Samples coordinates (regular radius)}
\usage{
coo.samplerr(coo, n)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Coo object.}

  \item{n}{an integer, the number fo points to sample.}
}
\value{
a matrix of (x,y) coordinates or an Coo object.
}
\description{
Samples n coordinates with a regular angle
}
\examples{
data(bot)
stack(bot)
bot <- coo.center(bot)
stack(coo.samplerr(bot, 12))
coo.plot(bot[4])
coo.plot(rr <- coo.samplerr(bot[4], 12))
cpos <- coo.centpos(bot[4])
segments(cpos[1], cpos[2], rr[, 1], rr[, 2])
}
\keyword{coo_utilities}

