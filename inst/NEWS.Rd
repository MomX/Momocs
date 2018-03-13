\name{NEWS}
\title{Momocs News}
% Borrowed from the inspiring vegan.
% Cheers to Jari Oksanen and others for the format!

\encoding{UTF-8}

\section{Changes in version 1.2.5 (CRAN + GitHub)}{
  \itemize{
    \item Moved everything to `github.com/MomX/Momocs`
    \item graphics is not dead (aka grindr): pipe-friendly base layers for biplots and shape drawing on cartesian coordinates. Is used to replace all multivariate plotters (eg `plot.PCA`), family pictures (eg `stack`, `panel`) and single shape plotters (eg `ldk_plot`, `coo_plot`). This strategy is faster, much more generic and will ease further development and maintenance compared to previous Momocs graphs.
    \item vignette for `grindr`
    \item ongoing complete review of code
    \item ongoing complete review of manual pages: lots of grouping, more and better examples, default values in `param`s
    \item released `reshape2`, `plyr` dependencies
    \item palettes are now those colorblind-friendly from RColorBrewer and those, state of the art, virids palettes. See also `pal_manual`, `pal_qual_solarized` and `pal_seq_grey`.
    \item `dispatch_fac` is now behind all `fac` arguments
    \item `fgProcrustes` now accepts lists
    \item new functions: `andnow` and `andnow_method` class tells you what to do with this object, and which classes are supported by this function/method.
    \item new `coo_*` methods: `coo_range` and `coo_range_enlarge`
    \item `coo` function ported to methods and now supporting `.Coo` directly: `coo_angle_edges`, `coo_angle_tangent`, `coo_boundingbox`, `coo_calliper`, `coo_chull`, `coo_chull_onion`, `coo_circularity`, `coo_circularityharalick`, `coo_circularity_norm`, `coo_convexity`, `coo_dxy`, `coo_eccentricityboundingbox`, `coo_eccentricityeigen`, `coo_elongation`, `coo_intersect_angle`, `coo_intersect_direction`, `coo_intersect_segment`, `coo_perim`, `coo_perimcum`, `coo_perimpts`, `coo_rectangularity`, `coo_rectilinearity`, `coo_scalex`, `coo_scaley`, `coo_solidity`, `coo_truss`.
    \item `truss` is now `coo_truss` and a method of it own.
    \item some non-exported functions (ie internals) are now homegeneously begin with a `.`, eg `.is.error` (try `Momocs:::. + <Tab>` for a complete list).
    \item some previously exported functions are now internals (`function_foo` renamed to `.function_foo`): `.coo_angle_edge1`, `.vecs_param`, `.refactor`
    \item Deprecated `plot3.PCA` (will be replaced in further versions); `Ntable` splitted into of `plot_table` + `table`; `coo_tangle` now in `coo_angle_tangent`; `coo_theta3` now in `coo_angle_edges`, `truss` now in `coo_truss` ; `plot.Coo` is now `inspect`
    \item Deprecated `stack2` and `panel2` before their rewriting.
    \item Deprecated `as_Out`, that should have been `efourier_i.OutCoe` anyway. Consequently deprecated `panel.OutCoe` method that was additionnaly the only `Coe` method. May be back in further versions.
    \item Waiting for a cleaner fix, `subset` is now `subsetize`...

    \item Deprecated all `is.*` aliases for `is_*` methods
    \item largely reorganized `.R` files
    \item fixed a bug in `LDA` when `retain=1` (#e7704eb)
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

