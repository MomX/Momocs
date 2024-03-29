% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gr-domesticplotters.R
\name{plot_devsegments}
\alias{plot_devsegments}
\title{Draws colored segments from a matrix of coordinates.}
\usage{
plot_devsegments(coo, cols, lwd = 1)
}
\arguments{
\item{coo}{A matrix of coordinates.}

\item{cols}{A vector of color of \code{length = nrow(coo)}.}

\item{lwd}{The \code{lwd} to use for drawing segments.}
}
\value{
a drawing on the last plot
}
\description{
Given a matrix of (x; y) coordinates, draws segments between every points
defined by the row of the matrix and uses a color to display an information.
}
\examples{

# we load some data
guinness <- coo_sample(bot[9], 100)

# we calculate the diff between 48 harm and one with 6 harm.
out.6    <- efourier_i(efourier(guinness, nb.h=6), nb.pts=120)

# we calculate deviations, you can also try 'edm'
dev <- edm_nearest(out.6, guinness) / coo_centsize(out.6)

# we prepare the color scale
d.cut <- cut(dev, breaks=20, labels=FALSE, include.lowest=TRUE)
cols  <- paste0(col_summer(20)[d.cut], 'CC')

# we draw the results
coo_plot(guinness, main='Guiness fitted with 6 harm.', points=FALSE)
par(xpd=NA)
plot_devsegments(out.6, cols=cols, lwd=4)
coo_draw(out.6, lty=2, points=FALSE, col=NA)
par(xpd=FALSE)
}
\seealso{
Other plotting functions: 
\code{\link{coo_arrows}()},
\code{\link{coo_draw}()},
\code{\link{coo_listpanel}()},
\code{\link{coo_lolli}()},
\code{\link{coo_plot}()},
\code{\link{coo_ruban}()},
\code{\link{ldk_chull}()},
\code{\link{ldk_confell}()},
\code{\link{ldk_contour}()},
\code{\link{ldk_labels}()},
\code{\link{ldk_links}()},
\code{\link{plot_table}()}
}
\concept{plotting functions}
