\name{coo.align.calliper}
\alias{coo.align.calliper}
\title{Aligns shapes along their calliper length
And returns them registered on bookstein coordinates.
See \link{coo.bookstein}.}
\usage{
coo.align.calliper(coo)
}
\arguments{
  \item{coo}{a matrix or a list of (x,y) coordinates or an
  Coo object.}
}
\value{
a matrix of (x,y) coordinates or an Coo object.
}
\description{
Aligns shapes along their calliper length And returns them
registered on bookstein coordinates. See
\link{coo.bookstein}.
}
\examples{
\dontrun{
data(bot)
b <- bot[4]
coo.plot(b)
coo.plot(coo.align.calliper(b))
bot.al <- coo.align.calliper(bot)
stack(bot.al)
}
}
\keyword{coo_utilities}

