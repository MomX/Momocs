Momocs, morphometrics using R
======

 * Momocs is an [R](http://cran.r-project.org/) package intended to ease and to popularize morphometrics, the quantitative analysis of form.
 * It aims at gathering the most common approaches: traditional morphometrics, global descriptors, open outlines, closed outlines and configuration of landmarks into the same, open-source, environment.
 * _And_ using the same, simple and easy-to-learn, open-source, and extendable grammar both for the user and programmers.
 * Momocs hinges on the core functions developed in the book _[Morphometrics with R](http://www.springer.com/statistics/life+sciences,+medicine+%26+health/book/978-0-387-77789-4)_ by [Julien Claude](http://www.isem.univ-montp2.fr/recherche/equipes/biologie-du-developpement-et-evolution/personnel/claude-julien/) (2008), wraps them into the same coding grammar, and extends them both graphically and statistically.


Features
--------
__Note__: Momocs is being extensively rewritten (from v0.2 available on [CRAN](http://cran.r-project.org/web/packages/Momocs/index.html) and published in [_Journal of Statistical Software_](http://www.jstatsoft.org/v56/i13/paper) to the version v0.9 available only on GitHub so far.
I plan a releaase of the version v0.9 before July 2014 and a release of v1.0 which will include all the features marked with an asterisk (*) below, before September, 2014.

* __Data acquisition__
 * Outline extraction
 * Landmark definition on outlines
* Import from .nts, .tps, PAST, .txt, .xls, .csv

* __Outline analysis__
 * Elliptical Fourier analysis
 * Radius Variation Fourier analysis
 * Tangent Angle Fourier analysis
 * Eigen shape analysis*
 * Calibration (shape reconstruction, deviation, harmonic power, etc.)
* __Open-outlines__
 * Natural (raw) polynomials
 * Orthogonal (Legendre) polynomials
 * Discrete Cosinus Transform*
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
 * Multivariate analysis of variance
 * Linear discriminant analysis (with morphospace)
 * Hierarchical clustering
 * Regression trees*
 * K-means*
 * Multidimensional scaling*
 * Allometry / modularity testing*

* __Graphical methods__
 * Morphospaces for all morphometrics methods (and for combined views*) x PCA, LDA
 * Thin plate splines and deformation grids
 * Family pictures and quick inspection of whole datasets
 * Many other morphometrics plots

Installation
--------

* From R:<br />
<code>
install.packages("Momocs")
library(Momocs)
</code>
* For the very last version: <br />
<code>
library(devtools) # You need to install it first using install.packages("devtools")
install_github(c("vbonhomme/Momocs", "vbonhomme"))
library(Momocs)
</code>

Tutorial
--------
* [A graphical introduction to Momocs](https://github.com/vbonhomme/Momocs-vignette/blob/master/a-graph-intro-Momocs.pdf)
* Blog posts, ref card, FAQ, to come.

Contact
--------
You are welcome to:

* submit suggestions and bug-reports at: https://github.com/vbonhomme/Momocs/issues
* send a pull request on: https://github.com/vbonhomme/Momocs/
* compose a friendly e-mail to: bonhomme.vincent@gmail.com if you would like to collaborate with me or if you have any problem with your scripts. This will benefit to everyone.
* have a look to my personnal webpage: http://www.vincentbonhomme.fr



