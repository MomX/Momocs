\name{coo.aligncalliper}
\alias{coo.aligncalliper}
\title{Aligns shapes along their calliper length
And returns them registered on bookstein coordinates.
See \link{coo.bookstein}.}
\usage{
coo.aligncalliper(coo)
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
coo.plot(coo.aligncalliper(b))
bot.al <- coo.aligncalliper(bot)
stack(bot.al)
}
}
\keyword{coo_utilities}

