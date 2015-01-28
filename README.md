[![Build Status](https://travis-ci.org/vbonhomme/Momocs.png?branch=master)](https://travis-ci.org/vbonhomme/Momocs)

Momocs, morphometrics using R
======

Momocs is an [R](http://cran.r-project.org/) package intended to ease and to popularize morphometrics, the quantitative analysis of form.

It aims at gathering the most common approaches: traditional morphometrics, global descriptors, open outlines, closed outlines and configuration of landmarks into the same, open-source, environment. _And_ using the same, simple and easy-to-learn, open-source, and extendable grammar both for the user and programmers.

Momocs hinges on the core functions developed in the must-have book _[Morphometrics with R](http://www.springer.com/statistics/life+sciences,+medicine+%26+health/book/978-0-387-77789-4)_ by [Julien Claude](http://www.isem.univ-montp2.fr/recherche/equipes/biologie-du-developpement-et-evolution/personnel/claude-julien/) (2008), wraps them into the same coding grammar, and extends them both graphically and statistically.

Use Momocs
--------
__Get the last version:__
* First get `devtools` with `install.packages("devtools")`
* Then, download and load the very last version with:
```
devtools::install_github("vbonhomme/Momocs", build_vignettes=TRUE)
library(Momocs)
````
* Some tutorials are on their way: `browseVignettes("Momocs")`. Same thing, [here](https://github.com/vbonhomme/Momocs/tree/master/vignettes)

__You are welcome to:__
* [propose ideas and report bugs](https://github.com/vbonhomme/Momocs/issues)
* offer your published data to the world
* ask for hotline and collaborate with me: bonhomme.vincent@gmail.com

Features
--------
* The current [CRAN](http://cran.r-project.org/web/packages/Momocs/index.html) version (0.2) has been published in [_Journal of Statistical Software_](http://www.jstatsoft.org/v56/i13/paper)
* Since then, Momocs has been almost entirely rewritten and considerably enlarged.
* A CRAN version will be released when most of the features planned below (*) will be part of Momocs.

* __Data acquisition__
 * Outline extraction
 * Landmark definition on outlines* (via [StereoMorph](https://github.com/aaronolsen/StereoMorph))
 * Open curves digitization with bezier curves* (via [StereoMorph](https://github.com/aaronolsen/StereoMorph))
 * Import from `.nts`*, `.tps`*, `PAST`, `.txt`, etc.

* __Outline analysis__
 * Elliptical Fourier analysis (`efourier`)
 * Radius Variation Fourier analysis (`rfourier`)
 * Tangent Angle Fourier analysis (`tfourier`)
 * Calibration for all methods (`calibrate_reconstructions`, `calibrate_deviations`, `calibrate_harmonicpower`)
* __Open-outlines__
 * Natural (raw) polynomials (`npoly`)
 * Orthogonal (Legendre) polynomials (`opoly`)
 * Discrete Cosinus Transform (`dfourier`)
 * Same calibration methods as above
* __Configuration of landmarks__
 * Full Generalized Procrustes Adjustment (`fgProcrustes`)
 * Resistant Fit Procrustes Adjustments*
 * Sliding semi-landmarks*
* __Traditional morphometrics and global shape descriptors__
 * Some built-in facilities for multivariate analysis
 * A long list of shape scalars (eg. eccentricity, rectilinearity, etc.)
* __Multivariate analysis__
 * Mean shape (groupwise) calculations
 * Principal component analysis
 * Multivariate analysis of variance + pairwise testing
 * Linear discriminant analysis
 * Hierarchical clustering
 * Regression trees*
 * K-means*
 * Multidimensional scaling*
 * Allometry / modularity testing*

* __Graphical methods__
 * Elegant ggplot2 plots (ongoing)
 * Morphological spaces for all morphometrics methods (and for combined views) : eg: PCA, LDA*, etc.
 * Thin plate splines and variation around deformation grids
 * Family pictures and quick inspection of whole datasets (`panel`, `stack`, `plot`, `radar`*)
 * Many other morphometrics plots such

Architecture
-------------
Here is a scheme of the Momocs' architecture:
![Momocs functionnal architecture](https://github.com/vbonhomme/Momocs-vignette/blob/master/vignettes/MomocsArch.png)

