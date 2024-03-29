% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gr-domesticplotters.R
\name{coo_lolli}
\alias{coo_lolli}
\title{Plots (lollipop) differences between two configurations}
\usage{
coo_lolli(coo1, coo2, pch = NA, cex = 0.5, ...)
}
\arguments{
\item{coo1}{A \code{list} or a \code{matrix} of coordinates.}

\item{coo2}{A \code{list} or a \code{matrix} of coordinates.}

\item{pch}{a pch for the points (default to NA)}

\item{cex}{a cex for the points}

\item{...}{optional parameters to fed \link{points} and \link{segments}.}
}
\value{
a drawing on the last plot
}
\description{
Draws 'lollipops' between two configurations.
}
\examples{
coo_lolli(coo_sample(olea[3], 50), coo_sample(olea[6], 50))
title("A nice title !")
}
\seealso{
Other plotting functions: 
\code{\link{coo_arrows}()},
\code{\link{coo_draw}()},
\code{\link{coo_listpanel}()},
\code{\link{coo_plot}()},
\code{\link{coo_ruban}()},
\code{\link{ldk_chull}()},
\code{\link{ldk_confell}()},
\code{\link{ldk_contour}()},
\code{\link{ldk_labels}()},
\code{\link{ldk_links}()},
\code{\link{plot_devsegments}()},
\code{\link{plot_table}()}
}
\concept{plotting functions}
