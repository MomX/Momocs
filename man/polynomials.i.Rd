\name{polynomials.i}
\alias{polynomials.i}
\title{Calculate shape from a polynomial model}
\usage{
polynomials.i(mod, x.pred, nb.pts = nrow(mod$model), baseline1x = -1,
  baseline2x = 1)
}
\arguments{
  \item{mod}{a lm model (see \link{polynomials}).}

  \item{x.pred}{the x-range on which to predict y-values
  (basically, a sequence of points along the the baseline
  range).}

  \item{nb.pts}{the number of points to predict. By default
  (and can't be higher) the number of points in the
  original shape.}

  \item{baseline1x}{(x; y) coordinates of the first point
  of the baseline.}

  \item{baseline2x}{(x; y) coordinates of the second point
  of the baseline.}
}
\value{
a matrix of (x; y) coordinates.
}
\description{
Returns a matrix of (x; y) coordinates when passed with a
model obtained with \link{polynomials}.
}
\keyword{morphoCore}

