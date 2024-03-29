% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cl-def-Opn.R
\name{Opn}
\alias{Opn}
\title{Builds an Opn object}
\usage{
Opn(x, fac = dplyr::tibble(), ldk = list())
}
\arguments{
\item{x}{\code{list} of matrices of (x; y) coordinates, or an array, or a data.frame (and friends)}

\item{fac}{(optionnal) a \code{data.frame} of factors and/or numerics
specifying the grouping structure}

\item{ldk}{(optionnal) \code{list} of landmarks as row number indices}
}
\value{
an \code{Opn} object
}
\description{
In Momocs, \code{Opn} classes objects are
lists of \bold{op}e\bold{n} outlines, with optionnal components,
on which generic methods such as plotting methods (e.g. \link{stack})
and specific methods (e.g. \link{npoly} can be applied.
\code{\link{Opn}} objects are primarily \code{\link{Coo}} objects.
}
\examples{
#Methods on Opn
methods(class=Opn)
# we load some open outlines. See ?olea for credits
olea
panel(olea)
# orthogonal polynomials
op <- opoly(olea, degree=5)
# we print the Coe
op
# Let's do a PCA on it
op.p <- PCA(op)
plot(op.p, 'domes')
plot(op.p, 'var')
# and now an LDA after a PCA
olda <- LDA(PCA(op), 'var')
# for CV table and others
olda
plot_LDA(olda)
}
\seealso{
Other classes: 
\code{\link{Coe}()},
\code{\link{Coo}()},
\code{\link{Ldk}()},
\code{\link{OpnCoe}()},
\code{\link{OutCoe}()},
\code{\link{Out}()},
\code{\link{TraCoe}()}
}
\concept{classes}
