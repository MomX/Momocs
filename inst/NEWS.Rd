\name{NEWS}
\title{Momocs News}
% Borrowed from the inspiring vegan.
% Cheers to Jari Oksanen and others for the format!

\encoding{UTF-8}

\section{Changes in version 1.2.5 (CRAN + GitHub)}{
  \itemize{
    \item cheap base biplots (aka cheapbabi): pipe-friendly base layers for biplots and shape drawing on cartesian coordinates. Is used to replace all multivariate plotters (eg `plot.PCA`), family pictures (eg `stack`, `panel`) and single shape plotters (eg `ldk_plot`, `coo_plot`). This strategy is faster, much more generic and will ease further development and maintenance.
    \item vignette for `cheapbabi`
    \item new `coo_*` methods: `coo_range` and `coo_range_enlarge`
    \item `Ntable` deprecated/splitted into of `plot_table` + `table`
    \item various minor bugs fixes, see GitHub
  }
}

\section{Changes in version 1.2.4 (GitHub only)}{
  \itemize{
    \item New functions/methods: coo_intersect_segment, coo_intersect_direction, coo_intersect_angle, def_ldk_direction, def_ldk_angle, def_ldk, tips, coo_sample_prop
    \item coo_slice.Opn now supports ldk.
    \item Dependencies: now depends rgeos for intersecting methods.
    \item Data: lightened `nsfishes` and charring to comply R CMD CHECK.
    \item various minor bugs fixes, see GitHub
  }
}

\section{Changes in version 1.2.3 (GitHub only)}{
  \itemize{
    \item Built with 3.4.3
    \item coo_slice now suports ldks
    \item various minor bugs fixes, see GitHub
  }
}

\section{Changes in version 1.2.2 (CRAN + GitHub)}{
  \itemize{
    \item MANOVA_PW now returns p-values
    \item new datasets: nsfishes
    \item minor debugging, see GitHub
  }
}

\section{Changes in version 1.2.1 (GitHub only)}{
  \itemize{
    \item some testing with testthat
    \item minor debugging, see GitHub
  }
}

\section{Changes in version 1.1.6}{
  \itemize{
    \item `sfourier` family implementation
    \item new datasets: `apodemus` and `mouse`
  }
}

\section{Changes in version 1.1.0}{
  \itemize{
    \item plot2.PCA deprecated due to ggplot2 2.2.0 breaking changes
    \item Few minor changes that can be followed on GitHub commits
    \item Development continues on GitHub.
  }
}

\section{Changes in version 1.0 "Mataa" (CRAN + GitHub)}{
  \itemize{
    \item \code{Mataa} release (\code{>=1.0}) on CRAN replaces the now completely obsolete \code{0.2.6}.
    \item It consists of the last version of \code{Barley} pushed to CRAN.
    \item Development continues on GitHub.
  }
}

\section{Changes in version 0.9 "Barley" (GitHub only)}{
  \itemize{
    \item \code{Barley} release with a complete rewriting of the package, and the inclusion of new morphometrics approches (open outlines, configuration of landmarks, global shape descriptors).
    \item New design with classes \link{Out}, \link{Opn} and \link{Ldk} to handle
      (closed) outlines, open outlines and configuration of landmarks. \link{Coo}
      becomes a "super class" encompassing the three others.
    \item S4 -> S3 rewriting. Maybe less orthodox but much more easy to
      understand, code, extend which is probably the most required for Momocs at this step.
    \item Renaming of functions/methods in a more consistent scheme
    \item New/partial rewriting of multivariate methods: MANOVA, MANOVA_PW, LDA, KMEANS, CLUST.
    \item Graphics have been refreshed: \link{panel}, \link{stack}, \link{plot.PCA},
    \item New datasets (chaff, flowers, oak, olea, molars, shapes, wings, etc.) to play around.
    \item More checking, tests, etc.
    \item A general review of the helpfiles
    \item Many issues fixed, see \code{https://github.com/vbonhomme/Momocs/issues}
    \item A tutorial as a vignette (see \code{browseVignette("Momocs")} is available
    \item A companion paper is on its way)
}



%  \subsection{BUG FIXES}{
%
%    \itemize{
%    \item item 1
%    \item item 2
%    }
%  } % bug fixes

%  \subsection{NEW FEATURES}{
%    \itemize{
%      \item item 1
%      \item item 2
%      }
%  } % new features

}

