[![Travis-CI Build Status](https://travis-ci.org/vbonhomme/Momocs.svg?branch=master)](https://travis-ci.org/vbonhomme/Momocs)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/Momocs)](http://cran.r-project.org/package=Momocs)
[![Coverage Status](https://img.shields.io/codecov/c/github/vbonhomme/Momocs/master.svg)](https://codecov.io/github/vbonhomme/Momocs?branch=master)

News
======
* Momocs version 1.0 "Mataa" :moyai: will be released on CRAN in March 2016
* Momocs' online documentation lives [there](http://vbonhomme.github.io/Momocsdoc/)


Momocs, morphometrics using [R](http://cran.r-project.org/)
======

Momocs:

* is a complete toolkit for morphometrics, from data extraction to multivariate analyses.
* includes most common 2D morphometrics approaches : outlines, open outlines, configurations of landmarks, traditional morphometrics, and facilities for data preparation, manipulation and visualization with a consistent grammar throughout.
* allows reproducible, complex morphometric analyses, paves the way for a pure open-source workflow in R, and other morphometrics approaches should be easy to plug in, or develop from, on top of this canvas.
* hinges on the core functions developed in the must-have book _[Morphometrics with R](http://www.springer.com/statistics/life+sciences,+medicine+%26+health/book/978-0-387-77789-4)_ by [Julien Claude](http://www.isem.univ-montp2.fr/recherche/equipes/biologie-du-developpement-et-evolution/personnel/claude-julien/) (2008).


Use it
--------

__CRAN version__

`install.packages("Momocs")`

__Last version__

That's always a good idea to use the last version.  
Be sure to have `devtools` installed (`install.packages("devtools")`), then:

`devtools::install_github("vbonhomme/Momocs", build_vignettes= TRUE)`

__How to cite it__ 

`citation("Momocs")`

__You are welcome to__   

* [propose ideas and report bugs](https://github.com/vbonhomme/Momocs/issues)
* offer your published data to the world
* ask for hotline and/or collaborate and/or hire me: `bonhomme.vincent@gmail.com`



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
* Sliding semi-landmarks

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

* Datasets for all types of data (`bot`, `trilo`, `mosquito`, `hearts`, `olea`, `shapes`, `wings`, `oak`, `molars`, `flower`, `chaff`, `charring`)
* Vignettes (`browseVignettes("Momocs")`)
* [Shiny](http://shiny.rstudio.com/) demonstrators/helpers. See [Momecs](https://github.com/vbonhomme/Momecs/)
* [Online documentation](http://vbonhomme.github.io/Momocs/)


Architecture
-------------
Here is a scheme of the Momocs' architecture:
![Momocs architecture](https://raw.githubusercontent.com/vbonhomme/Momocs/master/Momocs_arch.jpg)


