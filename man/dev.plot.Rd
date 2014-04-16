\name{dev.plot}
\alias{dev.plot}
\title{Plots deviation}
\usage{
dev.plot(mat, dev, cols, x=1:ncol(mat),
lines=TRUE, poly=TRUE, segments=FALSE, bw=0.1,
plot=FALSE, main="Deviation plot", xlab="", ylab="Deviations")
}
\arguments{
  \item{mat}{A \code{matrix} containing one or many lines
  (as individuals) with the corresponding y values (as
  cols).}

  \item{dev}{A \code{matrix} of the same dimension as mat
  but containing the deviation from the \code{mat} matrix.}

  \item{cols}{{A \code{vector} of \code{ncol(mat)}
  colors.}}

  \item{x}{An alternative vector of values for every column
  of \code{mat}.}

  \item{lines}{\code{logical}. Whether to draw lines for
  mean values.}

  \item{poly}{\code{logical}. Whether to draw polygons for
  mean + dev values.}

  \item{segments}{\code{logical}. Whether to draw segments
  for these mean + dev values.}

  \item{bw}{\code{numeric}. The width of the errors bars to
  draw.}

  \item{plot}{\code{logical}. Whether to plot a new
  graphical window.}

  \item{main}{\code{character}. A title for the plot.}

  \item{xlab}{\code{character}. A title for the x-axis.}

  \item{ylab}{\code{character}. A title for the y-axis.}
}
\description{
Calculates and plots series with associated error bars.
This function is used internally by methods based on
deviations for one one many outlines. Yet, it provides a
quick way to create plots of series, possibly with
deviations, from scratch.
}
\examples{
# we prepare some fake data
foo.mat  <- matrix(1:10, nr=3, nc=10, byrow=TRUE) + rnorm(30, sd=0.5)
foo.mat  <- foo.mat + matrix(rep(c(0, 2, 5), each=10), 3, byrow=TRUE)
foo.dev  <- matrix(abs(rnorm(30, sd=0.5)), nr=3, nc=10, byrow=TRUE)
# some possible tuning
dev.plot(foo.mat, plot=TRUE)
dev.plot(foo.mat, foo.dev, plot=TRUE)
dev.plot(foo.mat, foo.dev, lines=TRUE, plot=TRUE)
dev.plot(foo.mat, foo.dev, poly=FALSE, segments=TRUE, lines=TRUE, plot=TRUE)
dev.plot(foo.mat, foo.dev, cols=col.sari(3), poly=FALSE, segments=TRUE, lines=TRUE, plot=TRUE)
dev.plot(foo.mat, foo.dev, cols=col.summer(6)[4:6], plot=TRUE)
}
\keyword{graphics}

