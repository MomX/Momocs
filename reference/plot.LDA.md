# Plots Linear Discriminant Analysis

The Momocs' [`LDA`](http://momx.github.io/Momocs/reference/LDA.md)
plotter with many graphical options.

## Usage

``` r
# S3 method for class 'LDA'
plot(
  x,
  fac = x$fac,
  xax = 1,
  yax = 2,
  points = TRUE,
  col = "#000000",
  pch = 20,
  cex = 0.5,
  palette = col_solarized,
  center.origin = FALSE,
  zoom = 1,
  xlim = NULL,
  ylim = NULL,
  bg = par("bg"),
  grid = TRUE,
  nb.grids = 3,
  morphospace = FALSE,
  pos.shp = c("range", "full", "circle", "xy", "range_axes", "full_axes")[1],
  amp.shp = 1,
  size.shp = 1,
  nb.shp = 12,
  nr.shp = 6,
  nc.shp = 5,
  rotate.shp = 0,
  flipx.shp = FALSE,
  flipy.shp = FALSE,
  pts.shp = 60,
  border.shp = col_alpha("#000000", 0.5),
  lwd.shp = 1,
  col.shp = col_alpha("#000000", 0.95),
  stars = FALSE,
  ellipses = FALSE,
  conf.ellipses = 0.5,
  ellipsesax = TRUE,
  conf.ellipsesax = c(0.5, 0.9),
  lty.ellipsesax = 1,
  lwd.ellipsesax = sqrt(2),
  chull = FALSE,
  chull.lty = 1,
  chull.filled = FALSE,
  chull.filled.alpha = 0.92,
  density = FALSE,
  lev.density = 20,
  contour = FALSE,
  lev.contour = 3,
  n.kde2d = 100,
  delaunay = FALSE,
  loadings = FALSE,
  labelspoints = FALSE,
  col.labelspoints = par("fg"),
  cex.labelspoints = 0.6,
  abbreviate.labelspoints = TRUE,
  labelsgroups = TRUE,
  cex.labelsgroups = 0.8,
  rect.labelsgroups = FALSE,
  abbreviate.labelsgroups = FALSE,
  color.legend = FALSE,
  axisnames = TRUE,
  axisvar = TRUE,
  unit = FALSE,
  eigen = TRUE,
  rug = TRUE,
  title = substitute(x),
  box = TRUE,
  old.par = TRUE,
  ...
)
```

## Arguments

- x:

  an object of class "LDA", typically obtained with
  [LDA](http://momx.github.io/Momocs/reference/LDA.md)

- fac:

  name or the column id from the \$fac slot, or a formula combining
  colum names from the \$fac slot (cf. examples). A factor or a numeric
  of the same length can also be passed on the fly.

- xax:

  the first PC axis

- yax:

  the second PC axis

- points:

  logical whether to plot points

- col:

  a color for the points (either global, for every level of the fac or
  for every individual, see examples)

- pch:

  a pch for the points (either global, for every level of the fac or for
  every individual, see examples)

- cex:

  the size of the points

- palette:

  a [palette](http://momx.github.io/Momocs/reference/palettes.md)

- center.origin:

  logical whether to center the plot onto the origin

- zoom:

  to keep your distances

- xlim:

  numeric of length two ; if provided along with ylim, the x and y lims
  to use

- ylim:

  numeric of length two ; if provided along with xlim, the x and y lims
  to use

- bg:

  color for the background

- grid:

  logical whether to draw a grid

- nb.grids:

  and how many of them

- morphospace:

  logical whether to add the morphological space

- pos.shp:

  passed to
  [morphospace_positions](http://momx.github.io/Momocs/reference/morphospace_positions.md),
  one of `"range", "full", "circle", "xy", "range_axes", "full_axes"`.
  Or directly a matrix of positions. See
  [morphospace_positions](http://momx.github.io/Momocs/reference/morphospace_positions.md)

- amp.shp:

  amplification factor for shape deformation

- size.shp:

  the size of the shapes

- nb.shp:

  (pos.shp="circle") the number of shapes on the compass

- nr.shp:

  (pos.shp="full" or "range) the number of shapes per row

- nc.shp:

  (pos.shp="full" or "range) the number of shapes per column

- rotate.shp:

  angle in radians to rotate shapes (if several methods, a vector of
  angles)

- flipx.shp:

  same as above, whether to apply coo_flipx

- flipy.shp:

  same as above, whether to apply coo_flipy

- pts.shp:

  the number of points fro drawing shapes

- border.shp:

  the border color of the shapes

- lwd.shp:

  the line width for these shapes

- col.shp:

  the color of the shapes

- stars:

  logical whether to draw "stars"

- ellipses:

  logical whether to draw confidence ellipses

- conf.ellipses:

  numeric the quantile for the (bivariate gaussian) confidence ellipses

- ellipsesax:

  logical whether to draw ellipse axes

- conf.ellipsesax:

  one or more numeric, the quantiles for the (bivariate gaussian)
  ellipses axes

- lty.ellipsesax:

  if yes, the lty with which to draw these axes

- lwd.ellipsesax:

  if yes, one or more numeric for the line widths

- chull:

  logical whether to draw a convex hull

- chull.lty:

  if yes, its linetype

- chull.filled:

  logical whether to add filled convex hulls

- chull.filled.alpha:

  numeric alpha transparency

- density:

  whether to add a 2d density kernel estimation (based on kde2d)

- lev.density:

  if yes, the number of levels to plot (through
  [image](https://rdrr.io/r/graphics/image.html))

- contour:

  whether to add contour lines based on 2d density kernel

- lev.contour:

  if yes, the (approximate) number of lines to draw

- n.kde2d:

  the number of bins for kde2d, ie the 'smoothness' of density kernel

- delaunay:

  logical whether to add a delaunay 'mesh' between points

- loadings:

  logical whether to add loadings for every variables

- labelspoints:

  if TRUE rownames are used as labels, a colname from \$fac can also be
  passed

- col.labelspoints:

  a color for these labels, otherwise inherited from fac

- cex.labelspoints:

  a cex for these labels

- abbreviate.labelspoints:

  logical whether to abbreviate

- labelsgroups:

  logical whether to add labels for groups

- cex.labelsgroups:

  ifyes, a numeric for the size of the labels

- rect.labelsgroups:

  logical whether to add a rectangle behind groups names

- abbreviate.labelsgroups:

  logical, whether to abbreviate group names

- color.legend:

  logical whether to add a (cheap) color legend for numeric fac

- axisnames:

  logical whether to add PC names

- axisvar:

  logical whether to draw the variance they explain

- unit:

  logical whether to add plane unit

- eigen:

  logical whether to draw a plot of the eigen values

- rug:

  logical whether to add rug to margins

- title:

  character a name for the plot

- box:

  whether to draw a box around the plotting region

- old.par:

  whether to restore the old [par](https://rdrr.io/r/graphics/par.html).
  Set it to `FALSE` if you want to reuse the graphical window.

- ...:

  useless here, just to fit the generic plot

## Value

a plot

## Details

Widely inspired by the "layers" philosophy behind graphical functions of
the ade4 R package.

## Note

Morphospaces are deprecated so far. 99% of the code is shared with
[plot.PCA](http://momx.github.io/Momocs/reference/plot.PCA.md) waiting
for a general rewriting of a multivariate plotter. See
https://github.com/vbonhomme/Momocs/issues/121

## See also

[LDA](http://momx.github.io/Momocs/reference/LDA.md),
[plot_CV](http://momx.github.io/Momocs/reference/plot_CV.md),
[plot_CV2](http://momx.github.io/Momocs/reference/plot_CV2.md),
[plot.PCA](http://momx.github.io/Momocs/reference/plot.PCA.md).
