\name{panel}
\alias{panel}
\alias{panel.Out}
\title{Plot on Out objects: family picture}
\usage{
panel(Out, cols, borders, names, cex.names, ...)
}
\arguments{
  \item{Out}{The \code{Out} object to plot.}

  \item{cols}{A \code{vector} of colors for drawing the
  outlines. Either a single value or of length exactly
  equals to the number of coordinates.}

  \item{borders}{A \code{vector} of colors for drawing the
  borders. Either a single value or of length exactly
  equals to the number of coordinates.}

  \item{names}{whether to plot names or not. If TRUE uses
  shape names, otherwise pass a character for the names of
  the files}

  \item{cex.names}{a cex for the names}

  \item{...}{further arguments to be passed to
  \link{coo.list.panel}}
}
\description{
Plots all the outlines from a \code{Out} side by side
}
\examples{
data(mosquito)
panel(mosquito, names=TRUE, cex.names=0.5)
}
\seealso{
\link{stack.Out}, \link{plot.Out}.
}

