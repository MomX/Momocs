# Momocs 1.2.9+ (GitHub)
* new multivariate method: `KMEDOIDS` on top of `cluster::pam`. Added a `plot_silhouette` to go with this friend. Now depends `cluster`.
* new multivariate method: `NMDS` on top of `vegan::metaMDS`; use `plot_NMDS` to plot it and `vegan::stressplot` for a Shepard plot. Now depends `vegan`.
* new multivariate method: `MDS` on top of `cmdscale`; use `plot_MDS` to plot it.
* `mshapes` is now `MSHAPES` to stick with other capitalized "multivariate" methods. `mshapes` now just announces its future deprecation.
* new handling method `rm_missing` to deal with missing data in `$fac`
* Consequently, `plot_MSHAPES` is the new method for plotting it. Works on lists and on the result of `MSHAPES`.
* `plot_CV` has been slightly refreshed: jitter now always jitters the same way, and in a more compact radius around cell centers. This might be renamed to `plot_confusion` at some point.
* `fac_dispatcher` supports `NULL` which eases a lot multivariate plots (notably Momecs side)
* `TraCoe` class properly `data_frame`ize fac when build from `raCoe()`
* `CLUST` methods have been rewrote and now wraps around `dendextend`.
* Consequently released `ape` dependency.
* all morphometrics methods now accepts `list`s which is more elegant when working with `chop`+`combine`
* `LDA` methods have been partly rewritten and now handles constant and collinear variables by dropping them and storing them in the returned list
* morphospace on LDAs are (finally) back, yet still quite experimental.
* `coo_untiltx` now removes (residual) rotational biases after `coo_slidedirection` and should be used after it.
* `plot_LDA` now on. Pretty much the same as `plot_PCA` (which was expected yet nice).
* `.layerize_LDA` as an internal to prepare the previous
* `morphospace_position` and `chullfilled` in `plot_PCA` now properly working
* `verify` replaces `validate` to avoid conflict with `shiny::validate` (for Momecs)
* `subsetize` now exported (again)
* `def_ldk` gains a `close` and `points` argument
* no more printing of Coo errors that was due to some forgotten `data.frame` rather than `data_frame`
* fixed minor bugs (see GitHub history of commits)

# Momocs 1.2.9
### Preamble
* Started a general review of Momocs (including #184) to prepare for MomX. For my convenience, all changes will stack on 1.2.5 on GitHub but will appear, in the end, as 1.9.0 on CRAN to both reflect proximity with 2.0 and the huge quantity of changes since 1.2
* Moved everything to `github.com/MomX/Momocs`
* ongoing complete review of code
* ongoing complete review of manual pages: lots of grouping, more and better
* graphics is not dead (aka grindr): pipe-friendly base layers for biplots and shape drawing on cartesian coordinates. Is used to replace all multivariate plotters (eg `plot.PCA`), family pictures (eg `stack` will be replace by `pile` and remove an annoying conflict with `utils::stack`, `panel`) and single shape plotters (eg `ldk_plot`, `coo_plot`). This strategy is faster, much more generic and will ease further development and maintenance compared to previous Momocs graphs.
* A vignette details `grindr` rationale and use.

### New
* new functions: `andnow` and `andnow_method` class tells you what to do with this object, and which classes are supported by this function/method.
* new `coo_*` methods: `coo_range`, `coo_range_enlarge`, `coo_diffrange`, `coo_template_relatively`. The latter will prepare ground for proper size handling, notably for morphospaces.
* Many `coo` functions ported to methods and now supporting `.Coo` directly: `coo_angle_edges`, `coo_angle_tangent`, `coo_boundingbox`, `coo_calliper`, `coo_chull`, `coo_chull_onion`, `coo_circularity`, `coo_circularityharalick`, `coo_circularity_norm`, `coo_convexity`, `coo_dxy`, `coo_eccentricityboundingbox`, `coo_eccentricityeigen`, `coo_elongation`, `coo_intersect_angle`, `coo_intersect_direction`, `coo_intersect_segment`, `coo_perim`, `coo_perimcum`, `coo_perimpts`, `coo_rectangularity`, `coo_rectilinearity`, `coo_scalex`, `coo_scaley`, `coo_solidity`, `coo_truss`.
* Palettes are now those colorblind-friendly from RColorBrewer and those, state of the art, virids palettes. See also `pal_manual`, `pal_qual_solarized` and `pal_seq_grey`.
* `dispatch_fac` is now behind all `fac` arguments
* `fgProcrustes` now accepts lists
* `efourier` with default `norm=TRUE` now messages about how wrong it may be
* `dplyr::data_frame` everywhere pertinent

### Deprecated
* All `is.*` aliases for `is_*` methods
* Deprecated `classify` for a while
* All `calibrate_*(..., method)` have been renamed to `calibrate_*_method`. See `?calibrate_reconstructions` and friends.
* Deprecated `plot3.PCA` (will be replaced in further versions)
* `Ntable` now splitted into of `plot_table` + `table`
* `coo_tangle` now in `coo_angle_tangent`
* `coo_theta3` now in `coo_angle_edges`
* `truss` now in `coo_truss` and a method of its own
* `plot.Coo` is now `inspect`
* `pos.shapes` is now `morphospace_positions`
* `is_closed` is deprecated, now `coo_is_closed`; same for `is_open` now `coo_is_open`, to comply with all `coo_*` friends naming scheme
* `is_clockwise` is deprecated, now `coo_likely_clockwise`; same for `is_anticlockwise` now `coo_likely_anticlockwise`. Better reflect the incertainty and gather with `coo_*` friends
* Deprecated `table` (poor shortcut anyway and avoid this boring startup message)
* Deprecated `stack2` and `panel2` before their rewriting
* Deprecated `as_Out`, that should have been `efourier_i.OutCoe` anyway.
* Consequently deprecated `panel.OutCoe` method that was additionnaly the only `Coe` method. May be back in further versions.
* Some non-exported functions (ie internals) now homegeneously begin with a `.`, eg `.is.error` (try `Momocs:::. + <Tab>` for a complete list).
* Some previously exported functions are now internals (`function_foo` renamed to `.function_foo`): `.coo_angle_edge1`, `.vecs_param`, `.refactor`
* `NEWS` is now a decent NEWS file
* Online doc has been moved to [http://momx.github.io/Momocs/]

### Dependencies
* Released `reshape2`, `plyr` dependencies
* Now depends `RColorBrewer`, `progress`
* Proper indications of external functions with `::`. A nice side effect is to remove annoying messages when attaching Momocs. Another is the removal of most `importFrom`.

### Breaking changes
* Besides deprecated/renamed functions there should be no breaking changes.
* Future breaking changes are announced within concerned functions.

### Minor
* Waiting for a cleaner fix, `subset` is now `subsetize`...
* Fixed a bug in `LDA` when `retain=1` (#e7704eb)
* Messages homogeneity
* Internals have been lightened
* verbosity and progress bar are now handled via `options("verbose")`
* Various minor bugs fixes, see GitHub


# Momocs 1.2.4 (GitHub)
* New functions/methods: `coo_intersect_segment`, `coo_intersect_direction`, `coo_intersect_angle`, `def_ldk_direction`, `def_ldk_angle`, `def_ldk`, `def_ldk_tips`, `coo_sample_prop`
* `coo_slice.Opn` now supports `ldk` argument.
* Now depends rgeos for intersecting methods.
* Lightened `nsfishes` and `charring` to comply R CMD CHECK.
* Various minor bugs fixes, see GitHub


# Momocs 1.2.3 (GitHub)
* Built with R 3.4.3
* `coo_slice` now suports `ldk` argument
* Various minor bugs fixes, see GitHub


# Momocs 1.2.2 (CRAN + GitHub)
* `MANOVA_PW` now returns p-values
* New dataset `nsfishes`
* minor debugging, see GitHub

# Momocs 1.2.1 (GitHub)
* Introduced testing with `testthat`
* Minor debugging, see GitHub

# Momocs 1.1.6 (GitHub + CRAN)
* `sfourier` family implementation
* new datasets: `apodemus` and `mouse`

# Momocs 1.1.0 (GitHub + CRAN)
* `plot2.PCA` deprecated due to ggplot2 2.2.0 breaking changes
* Few minor changes that can be followed on GitHub commits

# Momocs 1.0 (CRAN + GitHub)
* Release on CRAN that replaces the now completely obsolete `0.2.6` (the one from the _JSS_ paper.
* It consists of the last version pushed to CRAN.

# Momocs 0.9 (Github)
* Started to routinely use GitHub (and `NEWS`)
* a complete rewriting of the package, and the inclusion of new morphometrics approches (open outlines, configuration of landmarks, global shape descriptors).
* New design with classes `Out`, `Opn` and `Ldk` to handle
(closed) outlines, open outlines and configuration of landmarks. `Coo` becomes a "super class" encompassing the three others.
* S4 -> S3 rewriting. Maybe less orthodox but much more easy to understand, code, extend which is probably the most required for Momocs at this step.
* Renaming of functions/methods in a more consistent scheme
* New/partial rewriting of multivariate methods: `MANOVA`, `MANOVA_PW`, `LDA`, `KMEANS`, `CLUST`.
* Graphics have been refreshed: `panel`, `stack`, `plot.PCA`
* New datasets: `chaff`, `flowers`, `oak`, `olea`, `molars`, `shapes`, `wings`,
* General review of the helpfiles
* Many issues fixed, see GitHub
* Momocs speed dating: a tutorial as a vignette (see `browseVignette("Momocs")` is available
