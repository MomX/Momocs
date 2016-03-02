<!--[![Travis-CI Build Status](https://travis-ci.org/vbonhomme/Momocs.svg?branch=master)](https://travis-ci.org/vbonhomme/Momocs)-->

News
======
* Momocs' documentation also lives [there](http://vbonhomme.github.io/Momocs/)
* :moyai: Momocs version 1.0 "Mataa" release on CRAN is planned for March 2016.

Momocs, morphometrics using R
======


Momocs is an [R](http://cran.r-project.org/) package intended to ease and to popularize morphometrics, the quantitative analysis of form. It aims at gathering the most common approaches: traditional morphometrics, global descriptors, open outlines, closed outlines and configuration of landmarks into the same, open-source, environment. _And_ using the same, simple and easy-to-learn, open-source, and extendable grammar both for the user and programmers. Momocs hinges on the core functions developed in the must-have book _[Morphometrics with R](http://www.springer.com/statistics/life+sciences,+medicine+%26+health/book/978-0-387-77789-4)_ by [Julien Claude](http://www.isem.univ-montp2.fr/recherche/equipes/biologie-du-developpement-et-evolution/personnel/claude-julien/) (2008), wraps them into the same coding grammar, and extends them both graphically and statistically.

__You are welcome to:__
* [propose ideas and report bugs](https://github.com/vbonhomme/Momocs/issues)
* offer your published data to the world
* ask for hotline and/or collaborate and/or hire me: bonhomme.vincent@gmail.com

Use Momocs
--------
* To install Momocs, first get `devtools` with `install.packages("devtools")` then, download and load the very last version with:
```
devtools::install_github("vbonhomme/Momocs", build_vignettes= TRUE)
library(Momocs)
````
* Please note that the current [CRAN](http://cran.r-project.org/package=Momocs) version (0.2) has been published in [_Journal of Statistical Software_](http://www.jstatsoft.org/v56/i13/paper) and is no longer supported. Since then, Momocs has been entirely rewritten and considerably enlarged.

Features
--------
(* = on its way)

__Data acquisition + Babel__
* Outline extraction
* Landmark definition on outlines (via [StereoMorph](https://github.com/aaronolsen/StereoMorph))
* Open curves digitization with bezier curves (via [StereoMorph](https://github.com/aaronolsen/StereoMorph))
* Import/Export from/to `.nts`, `.tps`, `PAST`, `.txt`, etc.*

__Outline analysis__
* Elliptical Fourier analysis
* Radius Variation Fourier analysis
* Tangent Angle Fourier analysis
* Calibration for all methods

__Open-outlines__
* Natural (raw) polynomials (`npoly`)
* Orthogonal (Legendre) polynomials (`opoly`)
* Discrete Cosinus Transform (`dfourier`)
* Calibration for all methods
* `bezier` core functions

__Configuration of landmarks__
* Full Generalized Procrustes Adjustment (`fgProcrustes`)
* Resistant Fit Procrustes Adjustments*
* Sliding semi-landmarks*

__Traditional morphometrics and global shape descriptors__
* Facilities for multivariate analysis (see `flowers`)
* A long list of shape scalars (eg. `coo_eccentricity`, `coo_rectilinearity`, etc.)

__Data handling__
* `filter`, `select`, `slice`, `mutate` and other verbs ala [dplyr](https://github.com/hadley/dplyr/)
* New verbs useful for morphometrics such as `combine` and `chop`, to handle several 2D views
* Permutation methods to resample data (`perm`, `breed`)
 
__Multivariate analysis__
* Mean shape (groupwise) calculations (`mshapes`)
* Principal component analysis (`PCA`)
* Multivariate analysis of variance (`MANOVA` + pairwise testing `MANOVA_PW`)
* Linear discriminant analysis and screening (`LDA`)
* Hierarchical clustering (`CLUST`)
* K-means (`KMEANS`)

__Graphical methods__
* Family pictures and quick inspection of whole datasets
* Some ggplot2 plots, when useful
* Morphological spaces for PCA
* Thin plate splines and variation around deformation grids

__Various__
* Datasets for all types of data (`bot`, `trilo`, `mosquito`, `hearts`, `olea`, `shapes`, `wings`, `oak`, `molars`, `flower`, `chaff`)
* Vignettes (`browseVignettes("Momocs")`)
* [Shiny](http://shiny.rstudio.com/) demonstrators/helpers*
* [Online documentation](http://vbonhomme.github.io/Momocs/)

<!--
Architecture
-------------
Here is a scheme of the Momocs' architecture:
![Momocs functionnal architecture](https://github.com/vbonhomme/Momocs-vignette/blob/master/vignettes/MomocsArch.png)

-->
