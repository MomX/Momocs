Momocs
--------
![maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)
[![Travis-CI Build Status](https://travis-ci.org/vbonhomme/Momocs.svg?branch=master)](https://travis-ci.org/vbonhomme/Momocs)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/Momocs)](http://cran.r-project.org/package=Momocs)
[![Coverage Status](https://img.shields.io/codecov/c/github/vbonhomme/Momocs/master.svg)](https://codecov.io/github/vbonhomme/Momocs?branch=master)
![CRAN downloads last month](http://cranlogs.r-pkg.org/badges/Momocs) ![CRAN downloads grand total](http://cranlogs.r-pkg.org/badges/grand-total/Momocs)

__:love_letter::love_letter::love_letter: Momocs is in a phase of very active development (see [#184](https://github.com/vbonhomme/Momocs/issues/184)).__

* is a complete toolkit for morphometrics, from data extraction to multivariate analyses.
* includes most common 2D morphometrics approaches : outlines, open outlines, configurations of landmarks, traditional morphometrics, and facilities for data preparation, manipulation and visualization with a consistent grammar throughout.
* allows reproducible, pipeable, complex morphometric analyses and paves the way for a pure open-source workflow in R, and other morphometrics approaches should be easy to plug in, or develop from, on top of this canvas.
* hinges on the core functions developed in the must-have book _[Morphometrics with R](http://www.springer.com/statistics/life+sciences,+medicine+%26+health/book/978-0-387-77789-4)_ by [Julien Claude](http://www.isem.univ-montp2.fr/recherche/equipes/biologie-du-developpement-et-evolution/personnel/claude-julien/) (2008).


* __Check__ the online doc and the tutorials [there](http://vbonhomme.github.io/Momocs/)
* __You're welcome to__ implement ideas, propose new ones, review the code, the helpfiles or the vignettes, report bugs, ask for help and propose to collaborate with me: [here on GitHub](https://github.com/vbonhomme/Momocs/issues) or there: `bonhomme.vincent@gmail.com`.
</b>

Use it
--------

From CRAN: `install.packages("Momocs")`

But I recommend using the last version:  

```
if (!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github('vbonhomme/Momocs')
```

You can cite it with: `citation("Momocs")`

<!--
Features
--------

__Matrices of xy-coordinates__
* ~100 generic tools like centering, scaling, rotating, calculating area, perimeter, etc. Full list with `apropos("coo_")`
* generic plotters: `coo_plot` and `g` (work in progress)

__Data acquisition + Babel__

* Outline extraction from black mask/silhouettes `.jpgs`
* Landmark definition on outlines (`def_ldk` or via [StereoMorph](https://github.com/aaronolsen/StereoMorph))
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

* Datasets for all types of data (`apodemus`, `bot`, `chaff`, `charring`, `flower`,  `hearts`, `molars`, `mosquito`, `mouse`, `oak`, `olea`, `shapes`, `trilo`, `wings`)
* [Shiny](http://shiny.rstudio.com/) demonstrators/helpers. See [Momecs](https://github.com/vbonhomme/Momecs/)
* [Online documentation](http://vbonhomme.github.io/Momocs/)

-->
