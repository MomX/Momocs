\name{NEWS}
\title{Momocs News}
% Borrowed from to the inspiring vegan.
% Cheers to Jari Oksanen and others!

\encoding{UTF-8}

\section{Changes in version 0.99 - "Barley"}{

"BArley" release with a complete rewriting of the package, and the inclusion of
new morphometrics approches (open outlines, configuration of landmarks, global
shape descriptors).

  \subsection{GENERAL}{
    \itemize{
      \item New design with classes \link{Out}, \link{Opn} and \link{Ldk} to handle
      (closed) outlines, open outlines and configuration of landmarks. \link{Coo}
      becomes a "super class" encompassing the three others.
      \item S4 -> S3 OO rewriting. Maybe less orthodox but much more easy to
      understand, code, extend which is probably the most required for Momocs at this step.
      \item New multivariate methods: \link{Manova} (rewriting), \link{LDA} (new),
      \link{clust} (rewriting), and more to come.
      \item Graphics have been refreshed: \link{panel}, \link{stack}, \link{plot.PCA},
      \link{plot.LDA} (along with morphospaces handling), the tps family, plus various
      minor improvements.
      }
  } % end general
  
  \subsection{ROAD TO 1.0}{
    \itemize{
      \item Test, test, test and users' feedback.
      \item Fill the gaps in the multivariate methods, documentations,
     manage the #todo tags and add comments everywhere in the code.
      \item Add other multivariate methods: multidimensional scaling, K-means,
      testing for allometry, modularity, multivariate regression, redundancy analysis,
      and multivariate regression.
      \item Procrustes resistant-fit and sliding semi-landmarks.
      \item Finish to write Momocs' vignette(s)
  }
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

