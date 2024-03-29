% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coo-utilities.R
\name{coo_trimtop}
\alias{coo_trimtop}
\title{Trims top coordinates from shape}
\usage{
coo_trimtop(coo, trim = 1)
}
\arguments{
\item{coo}{\code{matrix} of \verb{(x; y)} coordinates or any \link{Coo} object.}

\item{trim}{\code{numeric}, the number of coordinates to trim}
}
\value{
a trimmed shape
}
\description{
Removes \code{trim} coordinates from the top of a shape.
}
\examples{
olea[1] \%>\% coo_sample(12) \%T>\%
   print() \%T>\% ldk_plot() \%>\%
   coo_trimtop(4) \%T>\% print() \%>\% points(col="red")
}
\seealso{
Other coo_ utilities: 
\code{\link{coo_aligncalliper}()},
\code{\link{coo_alignminradius}()},
\code{\link{coo_alignxax}()},
\code{\link{coo_align}()},
\code{\link{coo_baseline}()},
\code{\link{coo_bookstein}()},
\code{\link{coo_boundingbox}()},
\code{\link{coo_calliper}()},
\code{\link{coo_centdist}()},
\code{\link{coo_center}()},
\code{\link{coo_centpos}()},
\code{\link{coo_close}()},
\code{\link{coo_down}()},
\code{\link{coo_dxy}()},
\code{\link{coo_extract}()},
\code{\link{coo_flipx}()},
\code{\link{coo_force2close}()},
\code{\link{coo_interpolate}()},
\code{\link{coo_is_closed}()},
\code{\link{coo_jitter}()},
\code{\link{coo_left}()},
\code{\link{coo_likely_clockwise}()},
\code{\link{coo_nb}()},
\code{\link{coo_perim}()},
\code{\link{coo_range}()},
\code{\link{coo_rev}()},
\code{\link{coo_right}()},
\code{\link{coo_rotatecenter}()},
\code{\link{coo_rotate}()},
\code{\link{coo_sample_prop}()},
\code{\link{coo_samplerr}()},
\code{\link{coo_sample}()},
\code{\link{coo_scale}()},
\code{\link{coo_shearx}()},
\code{\link{coo_slice}()},
\code{\link{coo_slidedirection}()},
\code{\link{coo_slidegap}()},
\code{\link{coo_slide}()},
\code{\link{coo_smoothcurve}()},
\code{\link{coo_smooth}()},
\code{\link{coo_template}()},
\code{\link{coo_trans}()},
\code{\link{coo_trimbottom}()},
\code{\link{coo_trim}()},
\code{\link{coo_untiltx}()},
\code{\link{coo_up}()},
\code{\link{is_equallyspacedradii}()}

Other coo_trimming functions: 
\code{\link{coo_trimbottom}()},
\code{\link{coo_trim}()}
}
\concept{coo_ utilities}
\concept{coo_trimming functions}
