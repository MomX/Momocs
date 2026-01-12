# Package index

## Morphometrics methods

- [`efourier()`](http://momx.github.io/Momocs/reference/efourier.md)
  [`efourier_norm()`](http://momx.github.io/Momocs/reference/efourier.md)
  : Elliptical Fourier transform (and its normalization)
- [`rfourier()`](http://momx.github.io/Momocs/reference/rfourier.md) :
  Radii variation Fourier transform (equally spaced radii)
- [`tfourier()`](http://momx.github.io/Momocs/reference/tfourier.md) :
  Tangent angle Fourier transform
- [`sfourier()`](http://momx.github.io/Momocs/reference/sfourier.md) :
  Radii variation Fourier transform (equally spaced curvilinear
  abscissa)
- [`dfourier()`](http://momx.github.io/Momocs/reference/dfourier.md) :
  Discrete cosinus transform
- [`opoly()`](http://momx.github.io/Momocs/reference/opoly.md) :
  Calculate orthogonal polynomial fits on open outlines
- [`npoly()`](http://momx.github.io/Momocs/reference/npoly.md) :
  Calculate natural polynomial fits on open outlines
- [`fgProcrustes()`](http://momx.github.io/Momocs/reference/fgProcrustes.md)
  : Full Generalized Procrustes alignment between shapes
- [`fgsProcrustes()`](http://momx.github.io/Momocs/reference/fgsProcrustes.md)
  : Full Generalized Procrustes alignment between shapes with sliding
  landmarks
- [`bezier()`](http://momx.github.io/Momocs/reference/bezier.md) :
  Calculates Bezier coefficients from a shape

## More morphometrics methods

Inverse functions and some others internally used by the main ones

- [`efourier_i()`](http://momx.github.io/Momocs/reference/efourier_i.md)
  : Inverse elliptical Fourier transform
- [`rfourier_i()`](http://momx.github.io/Momocs/reference/rfourier_i.md)
  : Inverse radii variation Fourier transform
- [`tfourier_i()`](http://momx.github.io/Momocs/reference/tfourier_i.md)
  : Inverse tangent angle Fourier transform
- [`sfourier_i()`](http://momx.github.io/Momocs/reference/sfourier_i.md)
  : Inverse radii variation Fourier transform
- [`dfourier_i()`](http://momx.github.io/Momocs/reference/dfourier_i.md)
  : Investe discrete cosinus transform
- [`opoly_i()`](http://momx.github.io/Momocs/reference/poly_i.md)
  [`npoly_i()`](http://momx.github.io/Momocs/reference/poly_i.md) :
  Calculates shape from a polynomial model
- [`bezier_i()`](http://momx.github.io/Momocs/reference/bezier_i.md) :
  Calculates a shape from Bezier coefficients
- [`fProcrustes()`](http://momx.github.io/Momocs/reference/fProcrustes.md)
  : Full Procrustes alignment between two shapes
- [`pProcrustes()`](http://momx.github.io/Momocs/reference/pProcrustes.md)
  : Partial Procrustes alignment between two shapes

## Calibration and other helpers for (open or not) outlines

When defined

- [`calibrate_deviations()`](http://momx.github.io/Momocs/reference/calibrate_deviations.md)
  [`calibrate_deviations_efourier()`](http://momx.github.io/Momocs/reference/calibrate_deviations.md)
  [`calibrate_deviations_tfourier()`](http://momx.github.io/Momocs/reference/calibrate_deviations.md)
  [`calibrate_deviations_rfourier()`](http://momx.github.io/Momocs/reference/calibrate_deviations.md)
  [`calibrate_deviations_sfourier()`](http://momx.github.io/Momocs/reference/calibrate_deviations.md)
  [`calibrate_deviations_npoly()`](http://momx.github.io/Momocs/reference/calibrate_deviations.md)
  [`calibrate_deviations_opoly()`](http://momx.github.io/Momocs/reference/calibrate_deviations.md)
  [`calibrate_deviations_dfourier()`](http://momx.github.io/Momocs/reference/calibrate_deviations.md)
  : Quantitative calibration, through deviations, for Out and Opn
  objects
- [`calibrate_harmonicpower()`](http://momx.github.io/Momocs/reference/calibrate_harmonicpower.md)
  [`calibrate_harmonicpower_efourier()`](http://momx.github.io/Momocs/reference/calibrate_harmonicpower.md)
  [`calibrate_harmonicpower_rfourier()`](http://momx.github.io/Momocs/reference/calibrate_harmonicpower.md)
  [`calibrate_harmonicpower_tfourier()`](http://momx.github.io/Momocs/reference/calibrate_harmonicpower.md)
  [`calibrate_harmonicpower_sfourier()`](http://momx.github.io/Momocs/reference/calibrate_harmonicpower.md)
  [`calibrate_harmonicpower_dfourier()`](http://momx.github.io/Momocs/reference/calibrate_harmonicpower.md)
  : Quantitative calibration, through harmonic power, for Out and Opn
  objects
- [`calibrate_reconstructions_efourier()`](http://momx.github.io/Momocs/reference/calibrate_reconstructions.md)
  [`calibrate_reconstructions_rfourier()`](http://momx.github.io/Momocs/reference/calibrate_reconstructions.md)
  [`calibrate_reconstructions_tfourier()`](http://momx.github.io/Momocs/reference/calibrate_reconstructions.md)
  [`calibrate_reconstructions_sfourier()`](http://momx.github.io/Momocs/reference/calibrate_reconstructions.md)
  [`calibrate_reconstructions_npoly()`](http://momx.github.io/Momocs/reference/calibrate_reconstructions.md)
  [`calibrate_reconstructions_opoly()`](http://momx.github.io/Momocs/reference/calibrate_reconstructions.md)
  [`calibrate_reconstructions_dfourier()`](http://momx.github.io/Momocs/reference/calibrate_reconstructions.md)
  : Calibrate using reconstructed shapes
- [`calibrate_r2()`](http://momx.github.io/Momocs/reference/calibrate_r2.md)
  [`calibrate_r2_opoly()`](http://momx.github.io/Momocs/reference/calibrate_r2.md)
  [`calibrate_r2_npoly()`](http://momx.github.io/Momocs/reference/calibrate_r2.md)
  : Quantitative r2 calibration for Opn objects
- [`harm_pow()`](http://momx.github.io/Momocs/reference/harm_pow.md) :
  Calculates harmonic power given a list from e/t/rfourier
- [`hcontrib()`](http://momx.github.io/Momocs/reference/harm.contrib.md)
  : Harmonic contribution to shape
- [`coeff_rearrange()`](http://momx.github.io/Momocs/reference/coeff_rearrange.md)
  : Rearrange a matrix of (typically Fourier) coefficients
- [`coeff_sel()`](http://momx.github.io/Momocs/reference/coeff_sel.md) :
  Helps to select a given number of harmonics from a numerical vector.
- [`coeff_split()`](http://momx.github.io/Momocs/reference/coeff_split.md)
  : Converts a numerical description of harmonic coefficients to a named
  list.
- [`rm_asym()`](http://momx.github.io/Momocs/reference/rm_asym.md)
  [`rm_sym()`](http://momx.github.io/Momocs/reference/rm_asym.md) :
  Removes asymmetric and symmetric variation on OutCoe objects
- [`symmetry()`](http://momx.github.io/Momocs/reference/symmetry.md) :
  Calcuates symmetry indices on OutCoe objects
- [`Ptolemy()`](http://momx.github.io/Momocs/reference/Ptolemy.md) :
  Ptolemaic ellipses and illustration of efourier
- [`morphospace_positions()`](http://momx.github.io/Momocs/reference/morphospace_positions.md)
  : Calculates nice positions on a plane for drawing shapes

## Classes

Their description and rationale

- [`Coo()`](http://momx.github.io/Momocs/reference/Coo.md) : Coo "super"
  class
- [`Out()`](http://momx.github.io/Momocs/reference/Out.md) : Builds an
  Out object
- [`Opn()`](http://momx.github.io/Momocs/reference/Opn.md) : Builds an
  Opn object
- [`Ldk()`](http://momx.github.io/Momocs/reference/Ldk.md) : Builds an
  Ldk object
- [`Coe()`](http://momx.github.io/Momocs/reference/Coe.md) : Coe "super"
  class
- [`OutCoe()`](http://momx.github.io/Momocs/reference/OutCoe.md) :
  Builds an OutCoe object
- [`OpnCoe()`](http://momx.github.io/Momocs/reference/OpnCoe.md) :
  Builds an OpnCoe object
- [`TraCoe()`](http://momx.github.io/Momocs/reference/TraCoe.md) :
  Traditional morphometrics class
- [`is_Coo()`](http://momx.github.io/Momocs/reference/is.md)
  [`is_PCA()`](http://momx.github.io/Momocs/reference/is.md)
  [`is_LDA()`](http://momx.github.io/Momocs/reference/is.md)
  [`is_Out()`](http://momx.github.io/Momocs/reference/is.md)
  [`is_Opn()`](http://momx.github.io/Momocs/reference/is.md)
  [`is_Ldk()`](http://momx.github.io/Momocs/reference/is.md)
  [`is_Coe()`](http://momx.github.io/Momocs/reference/is.md)
  [`is_OutCoe()`](http://momx.github.io/Momocs/reference/is.md)
  [`is_OpnCoe()`](http://momx.github.io/Momocs/reference/is.md)
  [`is_LdkCoe()`](http://momx.github.io/Momocs/reference/is.md)
  [`is_TraCoe()`](http://momx.github.io/Momocs/reference/is.md)
  [`is_shp()`](http://momx.github.io/Momocs/reference/is.md)
  [`is_fac()`](http://momx.github.io/Momocs/reference/is.md)
  [`is_ldk()`](http://momx.github.io/Momocs/reference/is.md)
  [`is_slidings()`](http://momx.github.io/Momocs/reference/is.md)
  [`is_links()`](http://momx.github.io/Momocs/reference/is.md) : Class
  and component testers

## Classes manipulation

Largely inspired from dplyr’s verbs

- [`arrange()`](http://momx.github.io/Momocs/reference/arrange.md) :
  Arrange rows by variables
- [`at_least()`](http://momx.github.io/Momocs/reference/at_least.md) :
  Retain groups with at least n shapes
- [`chop()`](http://momx.github.io/Momocs/reference/chop.md) : Split to
  several objects based on a factor
- [`combine()`](http://momx.github.io/Momocs/reference/combine.md) :
  Combine several objects
- [`dissolve()`](http://momx.github.io/Momocs/reference/dissolve.md) :
  Dissolve Coe objects
- [`filter()`](http://momx.github.io/Momocs/reference/filter.md) :
  Subset based on conditions
- [`mutate()`](http://momx.github.io/Momocs/reference/mutate.md) : Add
  new variables
- [`rescale()`](http://momx.github.io/Momocs/reference/rescale.md) :
  Rescale coordinates from pixels to real length units
- [`rename()`](http://momx.github.io/Momocs/reference/rename.md) :
  Rename columns by name
- [`select()`](http://momx.github.io/Momocs/reference/select.md) :
  Select columns by name
- [`rm_harm()`](http://momx.github.io/Momocs/reference/rm_harm.md) :
  Removes harmonics from Coe objects
- [`rm_uncomplete()`](http://momx.github.io/Momocs/reference/rm_uncomplete.md)
  : Remove shapes with incomplete slices
- [`rm_missing()`](http://momx.github.io/Momocs/reference/rm_missing.md)
  : Remove shapes with missing data in fac
- [`rw_fac()`](http://momx.github.io/Momocs/reference/rw_fac.md) :
  Renames levels on Momocs objects
- [`sample_frac()`](http://momx.github.io/Momocs/reference/sample_frac.md)
  : Sample a fraction of shapes
- [`sample_n()`](http://momx.github.io/Momocs/reference/sample_n.md) :
  Sample n shapes
- [`slice()`](http://momx.github.io/Momocs/reference/slice.md) : Subset
  based on positions
- [`subsetize()`](http://momx.github.io/Momocs/reference/subset.md) :
  Subsetize various Momocs objects
- [`verify()`](http://momx.github.io/Momocs/reference/verify.md) :
  Validates Coo objects

## Import/Export

Will be all moved to [Momit](http://www.github.com/MomX/Momit) soon

- [`export()`](http://momx.github.io/Momocs/reference/export.md) :
  Exports Coe objects and shapes
- [`as_df()`](http://momx.github.io/Momocs/reference/as_df.md) : Turn
  Momocs objects into tydy data_frames
- [`import_Conte()`](http://momx.github.io/Momocs/reference/import_Conte.md)
  : Extract outlines coordinates from an image silhouette
- [`import_jpg()`](http://momx.github.io/Momocs/reference/import_jpg.md)
  : Extract outline coordinates from multiple .jpg files
- [`import_jpg1()`](http://momx.github.io/Momocs/reference/import_jpg1.md)
  : Extract outline coordinates from a single .jpg file
- [`import_StereoMorph_curve1()`](http://momx.github.io/Momocs/reference/import_StereoMorph.md)
  [`import_StereoMorph_curve()`](http://momx.github.io/Momocs/reference/import_StereoMorph.md)
  [`import_StereoMorph_ldk1()`](http://momx.github.io/Momocs/reference/import_StereoMorph.md)
  [`import_StereoMorph_ldk()`](http://momx.github.io/Momocs/reference/import_StereoMorph.md)
  : Import files creates by StereoMorph into Momocs
- [`import_tps()`](http://momx.github.io/Momocs/reference/import_tps.md)
  [`tps2coo()`](http://momx.github.io/Momocs/reference/import_tps.md) :
  Import a tps file
- [`import_txt()`](http://momx.github.io/Momocs/reference/import_txt.md)
  : Import coordinates from a .txt file
- [`tie_jpg_txt()`](http://momx.github.io/Momocs/reference/tie_jpg_txt.md)
  : Binds .jpg outlines from .txt landmarks taken on them
- [`img_plot()`](http://momx.github.io/Momocs/reference/img_plot.md)
  [`img_plot0()`](http://momx.github.io/Momocs/reference/img_plot.md) :
  Plots a .jpg image
- [`lf_structure()`](http://momx.github.io/Momocs/reference/lf_structure.md)
  : bind_db.Coe \<- bind_db.Coo Extracts structure from filenames
- [`pix2chc()`](http://momx.github.io/Momocs/reference/babel.md)
  [`chc2pix()`](http://momx.github.io/Momocs/reference/babel.md) :
  Convert (x; y) coordinates to chaincoded coordinates

## Multivariate methods

Will be in [Momecs](http://www.github.com/MomX/Momecs) at some point,
and with KMEDOIDS, SVM, NNET and QDA

- [`PCA()`](http://momx.github.io/Momocs/reference/PCA.md)
  [`as_PCA()`](http://momx.github.io/Momocs/reference/PCA.md) :
  Principal component analysis on Coe objects
- [`LDA()`](http://momx.github.io/Momocs/reference/LDA.md) : Linear
  Discriminant Analysis on Coe objects
- [`MSHAPES()`](http://momx.github.io/Momocs/reference/MSHAPES.md) :
  Mean shape calculation for Coo, Coe, etc.
- [`MANOVA()`](http://momx.github.io/Momocs/reference/MANOVA.md) :
  Multivariate analysis of (co)variance on Coe objects
- [`MANOVA_PW()`](http://momx.github.io/Momocs/reference/MANOVA_PW.md) :
  Pairwise Multivariate analyses of variance
- [`KMEANS()`](http://momx.github.io/Momocs/reference/KMEANS.md) :
  KMEANS on PCA objects
- [`KMEDOIDS()`](http://momx.github.io/Momocs/reference/KMEDOIDS.md) :
  KMEDOIDS
- [`MDS()`](http://momx.github.io/Momocs/reference/MDS.md) : (Metric)
  multidimensional scaling
- [`NMDS()`](http://momx.github.io/Momocs/reference/NMDS.md) : Non
  metric multidimensional scaling
- [`CLUST()`](http://momx.github.io/Momocs/reference/CLUST.md) :
  Hierarchical clustering

## More on multivariate

Will be in [Momecs](http://www.github.com/MomX/Momecs) at some point

- [`PCcontrib()`](http://momx.github.io/Momocs/reference/PCcontrib.md) :
  Shape variation along PC axes
- [`flip_PCaxes()`](http://momx.github.io/Momocs/reference/flip_PCaxes.md)
  : Flips PCA axes
- [`scree()`](http://momx.github.io/Momocs/reference/scree.md)
  [`scree_min()`](http://momx.github.io/Momocs/reference/scree.md)
  [`scree_plot()`](http://momx.github.io/Momocs/reference/scree.md) :
  How many axes to retain this much of variance or trace ?
- [`rePCA()`](http://momx.github.io/Momocs/reference/rePCA.md) : "Redo"
  a PCA on a new Coe
- [`reLDA()`](http://momx.github.io/Momocs/reference/reLDA.md) : "Redo"
  a LDA on new data
- [`classification_metrics()`](http://momx.github.io/Momocs/reference/classification_metrics.md)
  : Calculate classification metrics on a confusion matrix
- [`which_out()`](http://momx.github.io/Momocs/reference/which_out.md) :
  Identify outliers
- [`plot(`*`<LDA>`*`)`](http://momx.github.io/Momocs/reference/plot.LDA.md)
  : Plots Linear Discriminant Analysis
- [`plot(`*`<PCA>`*`)`](http://momx.github.io/Momocs/reference/plot.PCA.md)
  : Plots Principal Component Analysis
- [`boxplot(`*`<OutCoe>`*`)`](http://momx.github.io/Momocs/reference/boxplot.OutCoe.md)
  : Boxplot of morphometric coefficients
- [`boxplot(`*`<PCA>`*`)`](http://momx.github.io/Momocs/reference/boxplot.PCA.md)
  : Boxplot on PCA objects

## Graphics

Because who does not love graphics?

- [`panel()`](http://momx.github.io/Momocs/reference/panel.Coo.md) :
  Family picture of shapes
- [`mosaic_engine()`](http://momx.github.io/Momocs/reference/mosaic.md)
  [`mosaic()`](http://momx.github.io/Momocs/reference/mosaic.md) : Plots
  mosaics of shapes.
- [`pile()`](http://momx.github.io/Momocs/reference/pile.md) : Graphical
  pile of shapes
- [`stack(`*`<Coo>`*`)`](http://momx.github.io/Momocs/reference/stack.Coo.md)
  [`stack(`*`<Ldk>`*`)`](http://momx.github.io/Momocs/reference/stack.Coo.md)
  : Family picture of shapes
- [`inspect()`](http://momx.github.io/Momocs/reference/inspect.md) :
  Graphical inspection of shapes
- [`plot_CV()`](http://momx.github.io/Momocs/reference/plot_CV.md) :
  Plots a cross-validation table as an heatmap
- [`plot_CV2()`](http://momx.github.io/Momocs/reference/plot_CV2.md) :
  Plots a cross-correlation table
- [`plot_LDA()`](http://momx.github.io/Momocs/reference/plot_LDA.md) :
  LDA plot using grindr layers
- [`plot_MSHAPES()`](http://momx.github.io/Momocs/reference/plot_MSHAPES.md)
  : Pairwise comparison of a list of shapes
- [`plot_NMDS()`](http://momx.github.io/Momocs/reference/plot_NMDS.md)
  [`plot_MDS()`](http://momx.github.io/Momocs/reference/plot_NMDS.md) :
  NMDS plot unsing grindr layers
- [`plot_PCA()`](http://momx.github.io/Momocs/reference/plot_PCA.md) :
  PCA plot using grindr layers
- [`plot_devsegments()`](http://momx.github.io/Momocs/reference/plot_devsegments.md)
  : Draws colored segments from a matrix of coordinates.
- [`plot_silhouette()`](http://momx.github.io/Momocs/reference/plot_silhouette.md)
  : Silhouette plot
- [`plot_table()`](http://momx.github.io/Momocs/reference/plot_table.md)
  : Plots confusion matrix of sample sizes within \$fac
- [`tps2d()`](http://momx.github.io/Momocs/reference/tps2d.md)
  [`tps_apply()`](http://momx.github.io/Momocs/reference/tps2d.md) :
  Thin Plate Splines for 2D data
- [`tps_arr()`](http://momx.github.io/Momocs/reference/tps_arr.md) :
  Deformation 'vector field' using Thin Plate Splines
- [`tps_grid()`](http://momx.github.io/Momocs/reference/tps_grid.md) :
  Deformation grids using Thin Plate Splines
- [`tps_iso()`](http://momx.github.io/Momocs/reference/tps_iso.md) :
  Deformation isolines using Thin Plate Splines.
- [`tps_raw()`](http://momx.github.io/Momocs/reference/tps_raw.md) :
  Vanilla Thin Plate Splines

## grindr Graphics

(base) graphics is not dead!

- [`paper()`](http://momx.github.io/Momocs/reference/papers.md)
  [`paper_white()`](http://momx.github.io/Momocs/reference/papers.md)
  [`paper_grid()`](http://momx.github.io/Momocs/reference/papers.md)
  [`paper_chess()`](http://momx.github.io/Momocs/reference/papers.md)
  [`paper_dots()`](http://momx.github.io/Momocs/reference/papers.md) :
  grindr papers for shape plots
- [`draw_polygon()`](http://momx.github.io/Momocs/reference/drawers.md)
  [`draw_outline()`](http://momx.github.io/Momocs/reference/drawers.md)
  [`draw_outlines()`](http://momx.github.io/Momocs/reference/drawers.md)
  [`draw_points()`](http://momx.github.io/Momocs/reference/drawers.md)
  [`draw_landmarks()`](http://momx.github.io/Momocs/reference/drawers.md)
  [`draw_lines()`](http://momx.github.io/Momocs/reference/drawers.md)
  [`draw_centroid()`](http://momx.github.io/Momocs/reference/drawers.md)
  [`draw_curve()`](http://momx.github.io/Momocs/reference/drawers.md)
  [`draw_curves()`](http://momx.github.io/Momocs/reference/drawers.md)
  [`draw_firstpoint()`](http://momx.github.io/Momocs/reference/drawers.md)
  [`draw_axes()`](http://momx.github.io/Momocs/reference/drawers.md)
  [`draw_ticks()`](http://momx.github.io/Momocs/reference/drawers.md)
  [`draw_labels()`](http://momx.github.io/Momocs/reference/drawers.md)
  [`draw_links()`](http://momx.github.io/Momocs/reference/drawers.md)
  [`draw_title()`](http://momx.github.io/Momocs/reference/drawers.md) :
  grindr drawers for shape plots
- [`layer_frame()`](http://momx.github.io/Momocs/reference/layers.md)
  [`layer_axes()`](http://momx.github.io/Momocs/reference/layers.md)
  [`layer_ticks()`](http://momx.github.io/Momocs/reference/layers.md)
  [`layer_grid()`](http://momx.github.io/Momocs/reference/layers.md)
  [`layer_box()`](http://momx.github.io/Momocs/reference/layers.md)
  [`layer_fullframe()`](http://momx.github.io/Momocs/reference/layers.md)
  [`layer_points()`](http://momx.github.io/Momocs/reference/layers.md)
  [`layer_ellipses()`](http://momx.github.io/Momocs/reference/layers.md)
  [`layer_ellipsesfilled()`](http://momx.github.io/Momocs/reference/layers.md)
  [`layer_ellipsesaxes()`](http://momx.github.io/Momocs/reference/layers.md)
  [`layer_chull()`](http://momx.github.io/Momocs/reference/layers.md)
  [`layer_chullfilled()`](http://momx.github.io/Momocs/reference/layers.md)
  [`layer_stars()`](http://momx.github.io/Momocs/reference/layers.md)
  [`layer_delaunay()`](http://momx.github.io/Momocs/reference/layers.md)
  [`layer_density()`](http://momx.github.io/Momocs/reference/layers.md)
  [`layer_labelpoints()`](http://momx.github.io/Momocs/reference/layers.md)
  [`layer_labelgroups()`](http://momx.github.io/Momocs/reference/layers.md)
  [`layer_rug()`](http://momx.github.io/Momocs/reference/layers.md)
  [`layer_histogram_2()`](http://momx.github.io/Momocs/reference/layers.md)
  [`layer_density_2()`](http://momx.github.io/Momocs/reference/layers.md)
  [`layer_title()`](http://momx.github.io/Momocs/reference/layers.md)
  [`layer_axesnames()`](http://momx.github.io/Momocs/reference/layers.md)
  [`layer_eigen()`](http://momx.github.io/Momocs/reference/layers.md)
  [`layer_axesvar()`](http://momx.github.io/Momocs/reference/layers.md)
  [`layer_legend()`](http://momx.github.io/Momocs/reference/layers.md) :
  grindr layers for multivariate plots
- [`layer_morphospace_PCA()`](http://momx.github.io/Momocs/reference/layers_morphospace.md)
  [`layer_morphospace_LDA()`](http://momx.github.io/Momocs/reference/layers_morphospace.md)
  : Morphospace layers

## Bridges

Convert between different classes

- [`l2m()`](http://momx.github.io/Momocs/reference/bridges.md)
  [`m2l()`](http://momx.github.io/Momocs/reference/bridges.md)
  [`d2m()`](http://momx.github.io/Momocs/reference/bridges.md)
  [`m2d()`](http://momx.github.io/Momocs/reference/bridges.md)
  [`l2a()`](http://momx.github.io/Momocs/reference/bridges.md)
  [`a2l()`](http://momx.github.io/Momocs/reference/bridges.md)
  [`a2m()`](http://momx.github.io/Momocs/reference/bridges.md)
  [`m2a()`](http://momx.github.io/Momocs/reference/bridges.md)
  [`m2ll()`](http://momx.github.io/Momocs/reference/bridges.md) :
  Convert between different classes

## Landmark helpers

- [`add_ldk()`](http://momx.github.io/Momocs/reference/add_ldk.md) :
  Adds new landmarks on Out and Opn objects
- [`coo_plot()`](http://momx.github.io/Momocs/reference/coo_plot.md)
  [`ldk_plot()`](http://momx.github.io/Momocs/reference/coo_plot.md) :
  Plots a single shape
- [`ldk_check()`](http://momx.github.io/Momocs/reference/ldk_check.md) :
  Checks 'ldk' shapes
- [`ldk_chull()`](http://momx.github.io/Momocs/reference/ldk_chull.md) :
  Draws convex hulls around landmark positions
- [`ldk_confell()`](http://momx.github.io/Momocs/reference/ldk_confell.md)
  : Draws confidence ellipses for landmark positions
- [`ldk_contour()`](http://momx.github.io/Momocs/reference/ldk_contour.md)
  : Draws kernel density contours around landmark
- [`ldk_labels()`](http://momx.github.io/Momocs/reference/ldk_labels.md)
  : Add landmarks labels
- [`ldk_links()`](http://momx.github.io/Momocs/reference/ldk_links.md) :
  Draws links between landmarks
- [`def_ldk()`](http://momx.github.io/Momocs/reference/def_ldk.md) :
  Defines new landmarks on Out and Opn objects
- [`def_ldk_angle()`](http://momx.github.io/Momocs/reference/def_ldk_angle.md)
  [`def_ldk_direction()`](http://momx.github.io/Momocs/reference/def_ldk_angle.md)
  : Add new landmarks based on angular positions
- [`def_ldk_tips()`](http://momx.github.io/Momocs/reference/def_ldk_tips.md)
  : Define tips as new landmarks
- [`def_links()`](http://momx.github.io/Momocs/reference/def_links.md) :
  Defines links between landmarks
- [`def_slidings()`](http://momx.github.io/Momocs/reference/def_slidings.md)
  : Defines sliding landmarks matrix
- [`links_all()`](http://momx.github.io/Momocs/reference/links_all.md) :
  Creates links (all pairwise combinations) between landmarks
- [`links_delaunay()`](http://momx.github.io/Momocs/reference/links_delaunay.md)
  : Creates links (Delaunay triangulation) between landmarks
- [`d()`](http://momx.github.io/Momocs/reference/d.md) : A wrapper to
  calculates euclidean distances between two points
- [`measure()`](http://momx.github.io/Momocs/reference/measure.md) :
  Measures shape descriptors
- [`coo_truss()`](http://momx.github.io/Momocs/reference/coo_truss.md) :
  Truss measurement
- [`slidings_scheme()`](http://momx.github.io/Momocs/reference/slidings_scheme.md)
  : Extracts partitions of sliding coordinates
- [`rearrange_ldk()`](http://momx.github.io/Momocs/reference/rearrange_ldk.md)
  : Rearrange, (select and reorder) landmarks to retain

## Getters/Setters for components of interest

- [`get_chull_area()`](http://momx.github.io/Momocs/reference/get_chull_area.md)
  [`get_chull_volume()`](http://momx.github.io/Momocs/reference/get_chull_area.md)
  : Calculates convex hull area/volume of PCA scores
- [`get_ldk()`](http://momx.github.io/Momocs/reference/get_ldk.md) :
  Retrieves landmarks coordinates
- [`get_pairs()`](http://momx.github.io/Momocs/reference/get_pairs.md) :
  Get paired individual on a Coe, PCA or LDA objects
- [`get_slidings()`](http://momx.github.io/Momocs/reference/get_slidings.md)
  : Extracts sliding landmarks coordinates

## Data

To play around with various data types

- [`apodemus`](http://momx.github.io/Momocs/reference/data_apodemus.md)
  : Data: Outline coordinates of Apodemus (wood mouse) mandibles
- [`bot`](http://momx.github.io/Momocs/reference/data_bot.md) : Data:
  Outline coordinates of beer and whisky bottles.
- [`chaff`](http://momx.github.io/Momocs/reference/data_chaff.md) :
  Data: Landmark and semilandmark coordinates on cereal glumes
- [`charring`](http://momx.github.io/Momocs/reference/data_charring.md)
  : Data: Outline coordinates from an experimental charring on cereal
  grains
- [`flower`](http://momx.github.io/Momocs/reference/data_flower.md) :
  Data: Measurement of iris flowers
- [`hearts`](http://momx.github.io/Momocs/reference/data_hearts.md) :
  Data: Outline coordinates of hand-drawn hearts
- [`molars`](http://momx.github.io/Momocs/reference/data_molars.md) :
  Data: Outline coordinates of 360 molars
- [`mosquito`](http://momx.github.io/Momocs/reference/data_mosquito.md)
  : Data: Outline coordinates of mosquito wings.
- [`mouse`](http://momx.github.io/Momocs/reference/data_mouse.md) :
  Data: Outline coordinates of mouse molars
- [`nsfishes`](http://momx.github.io/Momocs/reference/data_nsfishes.md)
  : Data: Outline coordinates of North Sea fishes
- [`oak`](http://momx.github.io/Momocs/reference/data_oak.md) : Data:
  Configuration of landmarks of oak leaves
- [`olea`](http://momx.github.io/Momocs/reference/data_olea.md) : Data:
  Outline coordinates of olive seeds open outlines.
- [`shapes`](http://momx.github.io/Momocs/reference/data_shapes.md) :
  Data: Outline coordinates of various shapes
- [`trilo`](http://momx.github.io/Momocs/reference/data_trilo.md) :
  Data: Outline coordinates of cephalic outlines of trilobite
- [`wings`](http://momx.github.io/Momocs/reference/data_wings.md) :
  Data: Landmarks coordinates of mosquito wings

## Farming shapes

Most of them will move to [Momfarm](http://www.github.com/MomX/Momfarm)
soon

- [`dfourier_shape()`](http://momx.github.io/Momocs/reference/dfourier_shape.md)
  : Calculates and draws 'dfourier' shapes
- [`efourier_shape()`](http://momx.github.io/Momocs/reference/efourier_shape.md)
  : Calculates and draw 'efourier' shapes.
- [`rfourier_shape()`](http://momx.github.io/Momocs/reference/rfourier_shape.md)
  : Calculates and draw 'rfourier' shapes.
- [`sfourier_shape()`](http://momx.github.io/Momocs/reference/sfourier_shape.md)
  : Calculates and draw 'sfourier' shapes.
- [`tfourier_shape()`](http://momx.github.io/Momocs/reference/tfourier_shape.md)
  : Calculates and draws 'tfourier' shapes.
- [`breed()`](http://momx.github.io/Momocs/reference/breed.md) : Jitters
  Coe (and others) objects
- [`perm()`](http://momx.github.io/Momocs/reference/perm.md) : Permutes
  and breed Coe (and others) objects

## Color palettes

Qualitative, diverging and sequential, all those colorblind friendly
from RColorBrewer, viridis and more.

- [`pal_alpha()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_manual()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_qual_solarized()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_seq_grey()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_div_BrBG()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_div_PiYG()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_div_PRGn()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_div_PuOr()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_div_RdBu()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_div_RdYlBu()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_qual_Dark2()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_qual_Paired()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_qual_Set2()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_seq_Blues()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_seq_BuGn()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_seq_BuPu()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_seq_GnBu()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_seq_Greens()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_seq_Greys()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_seq_Oranges()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_seq_OrRd()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_seq_PuBu()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_seq_PuBuGn()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_seq_PuRd()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_seq_Purples()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_seq_RdPu()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_seq_Reds()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_seq_YlGn()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_seq_YlGnBu()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_seq_YlOrBr()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_seq_YlOrRd()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_seq_magma()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_seq_inferno()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_seq_plasma()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_seq_viridis()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_qual()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_seq()`](http://momx.github.io/Momocs/reference/palettes.md)
  [`pal_div()`](http://momx.github.io/Momocs/reference/palettes.md) :
  Color palettes
- [`col_summer()`](http://momx.github.io/Momocs/reference/color_palettes.md)
  [`col_summer2()`](http://momx.github.io/Momocs/reference/color_palettes.md)
  [`col_spring()`](http://momx.github.io/Momocs/reference/color_palettes.md)
  [`col_autumn()`](http://momx.github.io/Momocs/reference/color_palettes.md)
  [`col_black()`](http://momx.github.io/Momocs/reference/color_palettes.md)
  [`col_solarized()`](http://momx.github.io/Momocs/reference/color_palettes.md)
  [`col_gallus()`](http://momx.github.io/Momocs/reference/color_palettes.md)
  [`col_qual()`](http://momx.github.io/Momocs/reference/color_palettes.md)
  [`col_heat()`](http://momx.github.io/Momocs/reference/color_palettes.md)
  [`col_hot()`](http://momx.github.io/Momocs/reference/color_palettes.md)
  [`col_cold()`](http://momx.github.io/Momocs/reference/color_palettes.md)
  [`col_sari()`](http://momx.github.io/Momocs/reference/color_palettes.md)
  [`col_india()`](http://momx.github.io/Momocs/reference/color_palettes.md)
  [`col_bw()`](http://momx.github.io/Momocs/reference/color_palettes.md)
  [`col_grey()`](http://momx.github.io/Momocs/reference/color_palettes.md)
  : Some color palettes
- [`col_transp()`](http://momx.github.io/Momocs/reference/color_transparency.md)
  [`col_alpha()`](http://momx.github.io/Momocs/reference/color_transparency.md)
  : Transparency helpers and palettes

## Misc

- [`Momocs-package`](http://momx.github.io/Momocs/reference/Momocs.md)
  [`Momocs`](http://momx.github.io/Momocs/reference/Momocs.md) : Momocs
- [`cpx2coo()`](http://momx.github.io/Momocs/reference/complex.md)
  [`coo2cpx()`](http://momx.github.io/Momocs/reference/complex.md) :
  Convert complex to/from cartesian coordinates
- [`ed()`](http://momx.github.io/Momocs/reference/ed.md) : Calculates
  euclidean distance between two points.
- [`edm()`](http://momx.github.io/Momocs/reference/edm.md) : Calculates
  euclidean distance every pairs of points in two matrices.
- [`edm_nearest()`](http://momx.github.io/Momocs/reference/edm_nearest.md)
  : Calculates the shortest euclidean distance found for every point of
  one matrix among those of a second.
- [`edi()`](http://momx.github.io/Momocs/reference/edi.md) : Calculates
  euclidean intermediate between two points.
- [`is_equallyspacedradii()`](http://momx.github.io/Momocs/reference/is_equallyspacedradii.md)
  : Tests if coordinates likely have equally spaced radii
- [`fac_dispatcher()`](http://momx.github.io/Momocs/reference/fac_dispatcher.md)
  : Brew and serve fac from Momocs object

## coo\_ toolbox

circa 100 helpers for 2D shapes and Coo objects

- [`coo_align()`](http://momx.github.io/Momocs/reference/coo_align.md) :
  Aligns coordinates
- [`coo_aligncalliper()`](http://momx.github.io/Momocs/reference/coo_aligncalliper.md)
  : Aligns shapes along their 'calliper length'
- [`coo_alignminradius()`](http://momx.github.io/Momocs/reference/coo_alignminradius.md)
  : Aligns shapes using their shortest radius
- [`coo_alignxax()`](http://momx.github.io/Momocs/reference/coo_alignxax.md)
  : Aligns shapes along the x-axis
- [`coo_angle_edges()`](http://momx.github.io/Momocs/reference/coo_angle_edges.md)
  : Calculates the angle of every edge of a shape
- [`coo_angle_tangent()`](http://momx.github.io/Momocs/reference/coo_angle_tangent.md)
  [`coo_tangle()`](http://momx.github.io/Momocs/reference/coo_angle_tangent.md)
  : Calculates the tangent angle along the perimeter of a shape
- [`coo_area()`](http://momx.github.io/Momocs/reference/coo_area.md) :
  Calculates the area of a shape
- [`coo_arrows()`](http://momx.github.io/Momocs/reference/coo_arrows.md)
  : Plots (lollipop) differences between two configurations
- [`coo_baseline()`](http://momx.github.io/Momocs/reference/coo_baseline.md)
  : Register new baselines
- [`coo_bookstein()`](http://momx.github.io/Momocs/reference/coo_bookstein.md)
  : Register Bookstein's coordinates
- [`coo_boundingbox()`](http://momx.github.io/Momocs/reference/coo_boundingbox.md)
  : Calculates coordinates of the bounding box
- [`coo_calliper()`](http://momx.github.io/Momocs/reference/coo_calliper.md)
  : Calculates the calliper length
- [`coo_centdist()`](http://momx.github.io/Momocs/reference/coo_centdist.md)
  : Returns the distance between everypoints and the centroid
- [`coo_center()`](http://momx.github.io/Momocs/reference/coo_center.md)
  [`coo_centre()`](http://momx.github.io/Momocs/reference/coo_center.md)
  : Centers coordinates
- [`coo_centpos()`](http://momx.github.io/Momocs/reference/coo_centpos.md)
  : Calculate centroid coordinates
- [`coo_centsize()`](http://momx.github.io/Momocs/reference/coo_centsize.md)
  : Calculates centroid size
- [`coo_check()`](http://momx.github.io/Momocs/reference/coo_check.md) :
  Checks shapes
- [`coo_chull()`](http://momx.github.io/Momocs/reference/coo_chull.md)
  [`coo_chull_onion()`](http://momx.github.io/Momocs/reference/coo_chull.md)
  : Calculates the (recursive) convex hull of a shape
- [`coo_circularity()`](http://momx.github.io/Momocs/reference/coo_circularity.md)
  [`coo_circularityharalick()`](http://momx.github.io/Momocs/reference/coo_circularity.md)
  [`coo_circularitynorm()`](http://momx.github.io/Momocs/reference/coo_circularity.md)
  : Calculates the Haralick's circularity of a shape
- [`coo_close()`](http://momx.github.io/Momocs/reference/coo_close.md)
  [`coo_unclose()`](http://momx.github.io/Momocs/reference/coo_close.md)
  : Closes/uncloses shapes
- [`coo_convexity()`](http://momx.github.io/Momocs/reference/coo_convexity.md)
  : Calculates the convexity of a shape
- [`coo_down()`](http://momx.github.io/Momocs/reference/coo_down.md) :
  coo_down Retains coordinates with negative y-coordinates
- [`coo_draw()`](http://momx.github.io/Momocs/reference/coo_draw.md) :
  Adds a shape to the current plot
- [`coo_draw_rads()`](http://momx.github.io/Momocs/reference/coo_draw_rads.md)
  : Draw radii to the current plot
- [`coo_dxy()`](http://momx.github.io/Momocs/reference/coo_dxy.md) :
  Calculate abscissa and ordinate on a shape
- [`coo_eccentricityeigen()`](http://momx.github.io/Momocs/reference/coo_eccentricity.md)
  [`coo_eccentricityboundingbox()`](http://momx.github.io/Momocs/reference/coo_eccentricity.md)
  : Calculates the eccentricity of a shape
- [`coo_elongation()`](http://momx.github.io/Momocs/reference/coo_elongation.md)
  : Calculates the elongation of a shape
- [`coo_extract()`](http://momx.github.io/Momocs/reference/coo_extract.md)
  : Extract coordinates from a shape
- [`coo_flipx()`](http://momx.github.io/Momocs/reference/coo_flip.md)
  [`coo_flipy()`](http://momx.github.io/Momocs/reference/coo_flip.md) :
  Flips shapes
- [`coo_force2close()`](http://momx.github.io/Momocs/reference/coo_force2close.md)
  : Forces shapes to close
- [`coo_interpolate()`](http://momx.github.io/Momocs/reference/coo_interpolate.md)
  : Interpolates coordinates
- [`coo_intersect_angle()`](http://momx.github.io/Momocs/reference/coo_intersect_angle.md)
  [`coo_intersect_direction()`](http://momx.github.io/Momocs/reference/coo_intersect_angle.md)
  : Nearest intersection between a shape and a segment specified with an
  angle
- [`coo_intersect_segment()`](http://momx.github.io/Momocs/reference/coo_intersect_segment.md)
  : Nearest intersection between a shape and a segment
- [`coo_is_closed()`](http://momx.github.io/Momocs/reference/coo_is_closed.md)
  [`is_open()`](http://momx.github.io/Momocs/reference/coo_is_closed.md)
  : Test if shapes are closed
- [`coo_jitter()`](http://momx.github.io/Momocs/reference/coo_jitter.md)
  : Jitters shapes
- [`coo_ldk()`](http://momx.github.io/Momocs/reference/coo_ldk.md) :
  Defines landmarks interactively
- [`coo_left()`](http://momx.github.io/Momocs/reference/coo_left.md) :
  Retains coordinates with negative x-coordinates
- [`coo_length()`](http://momx.github.io/Momocs/reference/coo_length.md)
  : Calculates the length of a shape
- [`coo_likely_clockwise()`](http://momx.github.io/Momocs/reference/coo_likely_clockwise.md)
  [`coo_likely_anticlockwise()`](http://momx.github.io/Momocs/reference/coo_likely_clockwise.md)
  : Tests if shapes are (likely) developping clockwise or anticlockwise
- [`coo_listpanel()`](http://momx.github.io/Momocs/reference/coo_listpanel.md)
  : Plots sets of shapes.
- [`coo_lolli()`](http://momx.github.io/Momocs/reference/coo_lolli.md) :
  Plots (lollipop) differences between two configurations
- [`coo_lw()`](http://momx.github.io/Momocs/reference/coo_lw.md) :
  Calculates length and width of a shape
- [`coo_nb()`](http://momx.github.io/Momocs/reference/coo_nb.md) :
  Counts coordinates
- [`coo_oscillo()`](http://momx.github.io/Momocs/reference/coo_oscillo.md)
  : Momocs' 'oscilloscope' for Fourier-based approaches
- [`coo_perimpts()`](http://momx.github.io/Momocs/reference/coo_perim.md)
  [`coo_perimcum()`](http://momx.github.io/Momocs/reference/coo_perim.md)
  [`coo_perim()`](http://momx.github.io/Momocs/reference/coo_perim.md) :
  Calculates perimeter and variations
- [`coo_plot()`](http://momx.github.io/Momocs/reference/coo_plot.md)
  [`ldk_plot()`](http://momx.github.io/Momocs/reference/coo_plot.md) :
  Plots a single shape
- [`coo_range()`](http://momx.github.io/Momocs/reference/coo_range.md)
  [`coo_range_enlarge()`](http://momx.github.io/Momocs/reference/coo_range.md)
  [`coo_diffrange()`](http://momx.github.io/Momocs/reference/coo_range.md)
  : Calculate coordinates range
- [`coo_rectangularity()`](http://momx.github.io/Momocs/reference/coo_rectangularity.md)
  : Calculates the rectangularity of a shape
- [`coo_rectilinearity()`](http://momx.github.io/Momocs/reference/coo_rectilinearity.md)
  : Calculates the rectilinearity of a shape
- [`coo_rev()`](http://momx.github.io/Momocs/reference/coo_rev.md) :
  Reverses coordinates
- [`coo_right()`](http://momx.github.io/Momocs/reference/coo_right.md) :
  Retains coordinates with positive x-coordinates
- [`coo_rotate()`](http://momx.github.io/Momocs/reference/coo_rotate.md)
  : Rotates coordinates
- [`coo_rotatecenter()`](http://momx.github.io/Momocs/reference/coo_rotatecenter.md)
  : Rotates shapes with a custom center
- [`coo_ruban()`](http://momx.github.io/Momocs/reference/coo_ruban.md) :
  Plots differences as (colored) segments aka a ruban
- [`coo_sample()`](http://momx.github.io/Momocs/reference/coo_sample.md)
  : Sample coordinates (among points)
- [`coo_sample_prop()`](http://momx.github.io/Momocs/reference/coo_sample_prop.md)
  : Sample a proportion of coordinates (among points)
- [`coo_samplerr()`](http://momx.github.io/Momocs/reference/coo_samplerr.md)
  : Samples coordinates (regular radius)
- [`coo_scalars()`](http://momx.github.io/Momocs/reference/coo_scalars.md)
  : Calculates all scalar descriptors of shape
- [`coo_scale()`](http://momx.github.io/Momocs/reference/coo_scale.md)
  [`coo_scalex()`](http://momx.github.io/Momocs/reference/coo_scale.md)
  [`coo_scaley()`](http://momx.github.io/Momocs/reference/coo_scale.md)
  : Scales coordinates
- [`coo_shearx()`](http://momx.github.io/Momocs/reference/coo_shear.md)
  [`coo_sheary()`](http://momx.github.io/Momocs/reference/coo_shear.md)
  : Shears shapes
- [`coo_slice()`](http://momx.github.io/Momocs/reference/coo_slice.md) :
  Slices shapes between successive coordinates
- [`coo_slide()`](http://momx.github.io/Momocs/reference/coo_slide.md) :
  Slides coordinates
- [`coo_slidedirection()`](http://momx.github.io/Momocs/reference/coo_slidedirection.md)
  : Slides coordinates in a particular direction
- [`coo_slidegap()`](http://momx.github.io/Momocs/reference/coo_slidegap.md)
  : Slides coordinates using the widest gap
- [`coo_smooth()`](http://momx.github.io/Momocs/reference/coo_smooth.md)
  : Smoothes coordinates
- [`coo_smoothcurve()`](http://momx.github.io/Momocs/reference/coo_smoothcurve.md)
  : Smoothes coordinates on curves
- [`coo_solidity()`](http://momx.github.io/Momocs/reference/coo_solidity.md)
  : Calculates the solidity of a shape
- [`coo_tac()`](http://momx.github.io/Momocs/reference/coo_tac.md) :
  Calculates the total absolute curvature of a shape
- [`coo_template()`](http://momx.github.io/Momocs/reference/coo_template.md)
  [`coo_template_relatively()`](http://momx.github.io/Momocs/reference/coo_template.md)
  : 'Templates' shapes
- [`coo_trans()`](http://momx.github.io/Momocs/reference/coo_trans.md) :
  Translates coordinates
- [`coo_trim()`](http://momx.github.io/Momocs/reference/coo_trim.md) :
  Trims both ends coordinates from shape
- [`coo_trimbottom()`](http://momx.github.io/Momocs/reference/coo_trimbottom.md)
  : Trims bottom coordinates from shape
- [`coo_trimtop()`](http://momx.github.io/Momocs/reference/coo_trimtop.md)
  : Trims top coordinates from shape
- [`coo_truss()`](http://momx.github.io/Momocs/reference/coo_truss.md) :
  Truss measurement
- [`coo_untiltx()`](http://momx.github.io/Momocs/reference/coo_untiltx.md)
  : Removes rotation so that the centroid and a given point are parallel
  to the x-axis
- [`coo_up()`](http://momx.github.io/Momocs/reference/coo_up.md) :
  Retains coordinates with positive y-coordinates
- [`coo_width()`](http://momx.github.io/Momocs/reference/coo_width.md) :
  Calculates the width of a shape
