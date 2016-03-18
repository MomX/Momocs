\name{NEWS}
\title{Momocs News}
% Borrowed from the inspiring vegan.
% Cheers to Jari Oksanen and others!

\encoding{UTF-8}

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

