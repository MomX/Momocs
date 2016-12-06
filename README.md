[![Travis-CI Build Status](https://travis-ci.org/vbonhomme/Momocs.svg?branch=master)](https://travis-ci.org/vbonhomme/Momocs)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/Momocs)](http://cran.r-project.org/package=Momocs)
[![Coverage Status](https://img.shields.io/codecov/c/github/vbonhomme/Momocs/master.svg)](https://codecov.io/github/vbonhomme/Momocs?branch=master)    
CRAN downloads:    
![CRAN downloads last month](http://cranlogs.r-pkg.org/badges/Momocs)
![CRAN downloads grand total](http://cranlogs.r-pkg.org/badges/grand-total/Momocs)

Momocs:
========

* is a complete toolkit for morphometrics, from data extraction to multivariate analyses.
* includes most common 2D morphometrics approaches : outlines, open outlines, configurations of landmarks, traditional morphometrics, and facilities for data preparation, manipulation and visualization with a consistent grammar throughout.
* allows reproducible, pipeable, complex morphometric analyses and paves the way for a pure open-source workflow in R, and other morphometrics approaches should be easy to plug in, or develop from, on top of this canvas.
* hinges on the core functions developed in the must-have book _[Morphometrics with R](http://www.springer.com/statistics/life+sciences,+medicine+%26+health/book/978-0-387-77789-4)_ by [Julien Claude](http://www.isem.univ-montp2.fr/recherche/equipes/biologie-du-developpement-et-evolution/personnel/claude-julien/) (2008).

There is an exhaustive tutorial tha can be accessed [there](https://rawgit.com/vbonhomme/Momocs/master/inst/doc/Momocs_speed_dating.html) or with `browseVignettes("Momocs")`, directly on your machine.

Use it
--------

From CRAN: 

`install.packages("Momocs")`

But I recommend using the last version:  

```
if (!require(devtools)) {
    install.packages('devtools')
}
devtools::install_github('vbonhomme/Momocs')
```

You can cite it with:

`citation("Momocs")`

**Please feel free to implement ideas, propose new ones, review helpfiles, write vignettes, report bugs, ask for help : [just fill an issue](https://github.com/vbonhomme/Momocs/issues)**.

Features
--------

__Data acquisition + Babel__

* Outline extraction
* Landmark definition on outlines (via [StereoMorph](https://github.com/aaronolsen/StereoMorph))
* Open curves digitization with bezier curves (via [StereoMorph](https://github.com/aaronolsen/StereoMorph))
* Import/Export from/to `.nts`, `.tps`, `PAST`, `.txt`, etc.

__Outline analysis__

* Elliptical Fourier analysis (`efourier`)
* Radii variation (`rfourier`)
* Radii variation - curvilinear abscissa (`sfourier`)
* Tangent Angle Fourier analysis (`tfourier`)

__Open-outlines__

* Natural (raw) polynomials (`npoly`)
* Orthogonal (Legendre) polynomials (`opoly`)
* Discrete Cosinus Transform (`dfourier`)
* `bezier` core functions

__Configuration of landmarks__

* Full Generalized Procrustes Adjustment (`fgProcrustes`)
* Sliding semi-landmarks (`fgsProcrustes`)

__Traditional morphometrics and global shape descriptors__

* Facilities for multivariate analysis (see `flowers`)
* A long list of shape scalars (eg. `coo_eccentricity`, `coo_rectilinearity`, etc.)

__Data handling__

* Easy data manipulation with `filter`, `select`, `slice`, `mutate` and other verbs ala [dplyr](https://github.com/hadley/dplyr/)
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

* Family pictures and quick inspection of whole datasets (`stack` and `panel`)
* Some `ggplot2` plots, when useful (and convet Momocs' objects into `data.frames it with `as_df`)
* Morphological spaces for PCA
* Thin plate splines and variation around deformation grids


__Misc__

* Datasets for all types of data (`bot`, `trilo`, `mosquito`, `hearts`, `olea`, `shapes`, `wings`, `oak`, `molars`, `flower`, `chaff`, `charring`)
* [Shiny](http://shiny.rstudio.com/) demonstrators/helpers. See [Momecs](https://github.com/vbonhomme/Momecs/)
* [Online documentation](http://vbonhomme.github.io/Momocs/)
