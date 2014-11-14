Momocs, morphometrics using R
======

Momocs is an [R](http://cran.r-project.org/) package intended to ease and to popularize morphometrics, the quantitative analysis of form.
It aims at gathering the most common approaches: traditional morphometrics, global descriptors, open outlines, closed outlines and configuration of landmarks into the same, open-source, environment. _And_ using the same, simple and easy-to-learn, open-source, and extendable grammar both for the user and programmers. Momocs hinges on the core functions developed in the book _[Morphometrics with R](http://www.springer.com/statistics/life+sciences,+medicine+%26+health/book/978-0-387-77789-4)_ by [Julien Claude](http://www.isem.univ-montp2.fr/recherche/equipes/biologie-du-developpement-et-evolution/personnel/claude-julien/) (2008), wraps them into the same coding grammar, and extends them both graphically and statistically.

Installation
--------

For the very last version: 
```
library(devtools)
install_github("vbonhomme/Momocs", build_vignettes=TRUE)
library(Momocs)
````
* Have a look to the tutorials with `browseVignettes("Momocs")`.
* Same thing, [here](https://github.com/vbonhomme/Momocs/tree/master/vignettes)

![Momocs functionnal architecture](https://github.com/vbonhomme/Momocs-vignette/blob/master/vignettes/MomocsArch.png)


Next big steps
--------
* Fill the gaps (particularly for landmarks)
* Unify all plots (design), function (arguments consistency), etc.
* Review the helpfiles, finish the vignettes work
* Release Momocs 1.0 (Spring 2015)

How can you help and find help ?
--------

__You are welcome to:__
* test Momocs and send me your feedback
* submit suggestions and bug-reports
* ask for hotline
* collaborate with me
* just send me an email: bonhomme.vincent@gmail.com

Enjoy Momocs!

Features
--------
__Note__: Momocs is being extensively rewritten (from v0.2 available on [CRAN](http://cran.r-project.org/web/packages/Momocs/index.html) and published in [_Journal of Statistical Software_](http://www.jstatsoft.org/v56/i13/paper) to the version v0.9 available only (see above) so far, planned for extensive testing. A v1.0 which will include all the features marked with an asterisk (*) below, is planned for ~ September, 2014.

* __Data acquisition__
 * Outline extraction
 * Landmark definition on outlines
 * Import from .nts, .tps, PAST, .txt, .xls, .csv

* __Outline analysis__
 * Elliptical Fourier analysis
 * Radius Variation Fourier analysis
 * Tangent Angle Fourier analysis
 * Calibration (shape reconstruction, deviation, harmonic power, etc.)
* __Open-outlines__
 * Natural (raw) polynomials
 * Orthogonal (Legendre) polynomials
 * Discrete Cosinus Transform (core funct. available though)
 * Same calibration methods as above
* __Configuration of landmarks__
 * Full Generalized Procrustes Adjustment
 * Resistant Fit Procrustes Adjustments*
 * Sliding semi-landmarks*
* __Traditional morphometrics and global shape descriptors__
 * Some built-in facilities for multivariate analysis
 * A long list of shape scalars (eg. eccentricity, rectilinearity, etc.)
* __Multivariate analysis__
 * Mean shape (groupwise) calculations
 * Principal component analysis (with morphospace)
 * Multivariate analysis of variance + pairwise testing
 * Linear discriminant analysis (with morphospace)
 * Hierarchical clustering
 * Regression trees*
 * K-means*
 * Multidimensional scaling*
 * Allometry / modularity testing*

* __Graphical methods__
 * Morphospaces for all morphometrics methods (and for combined views*) x PCA, LDA
 * Thin plate splines and variation around deformation grids
 * Family pictures and quick inspection of whole datasets
 * Many other morphometrics plots
