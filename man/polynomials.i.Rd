\name{polynomials.i}
\alias{polynomials.i}
\title{Calculate shape from a polynomial model}
\usage{
polynomials.i(mod, x.pred, nb.pts = nrow(mod$model))
}
\arguments{
  \item{mod}{a lm model (see \link{polynomials}).}

  \item{x.pred}{the x-range on which to predict y-values
  (basically, a sequence of points along the the baseline
  range).}

  \item{nb.pts}{the number of points to predict. By default
  (and can't be higher) the number of points in the
  original shape.}
}
\value{
a matrix of (x; y) coordinates.
}
\description{
Returns a matrix of (x; y) coordinates when passed with a
model obtained with \link{polynomials}.
}
\keyword{morphoCore}

