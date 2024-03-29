% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coo-utilities.R
\name{coo_interpolate}
\alias{coo_interpolate}
\title{Interpolates coordinates}
\usage{
coo_interpolate(coo, n)
}
\arguments{
\item{coo}{\code{matrix} of \verb{(x; y)} coordinates or any \link{Coo} object.}

\item{n}{\code{integer}, the number fo points to interpolate.}
}
\value{
a \code{matrix} of (x; y) coordinates, or a \link{Coo} object.
}
\description{
Interpolates n coordinates 'among existing points'between' existing points,
along the perimeter of the coordinates provided and keeping the first point
}
\examples{
b5 <- bot \%>\% slice(1:5) # for speed sake
stack(b5)
stack(coo_scale(b5))
stack(b5)
stack(coo_interpolate(coo_sample(b5, 12), 120))
coo_plot(bot[1])
coo_plot(coo_interpolate(coo_sample(bot[1], 12), 120))
}
\seealso{
Other sampling functions: 
\code{\link{coo_extract}()},
\code{\link{coo_sample_prop}()},
\code{\link{coo_samplerr}()},
\code{\link{coo_sample}()}

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
\code{\link{coo_trimtop}()},
\code{\link{coo_trim}()},
\code{\link{coo_untiltx}()},
\code{\link{coo_up}()},
\code{\link{is_equallyspacedradii}()}
}
\concept{coo_ utilities}
\concept{sampling functions}
