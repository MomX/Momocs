# grindr layers for multivariate plots

Useful layers for building custom mutivariate plots using the cheapbabi
approach. See examples.

## Usage

``` r
layer_frame(x, center_origin = TRUE, zoom = 0.9)

layer_axes(x, col = "#999999", lwd = 1/2, ...)

layer_ticks(x, col = "#333333", cex = 3/4, lwd = 3/4, ...)

layer_grid(x, col = "#999999", lty = 3, grid = 3, ...)

layer_box(x, border = "#e5e5e5", ...)

layer_fullframe(x, ...)

layer_points(x, pch = 20, cex = 4/log1p(nrow(x$xy)), transp = 0, ...)

layer_ellipses(x, conf = 0.5, lwd = 1, alpha = 0, ...)

layer_ellipsesfilled(x, conf = 0.5, lwd = 1, alpha = 0, ...)

layer_ellipsesaxes(x, conf = 0.5, lwd = 1, alpha = 0, ...)

layer_chull(x, ...)

layer_chullfilled(x, alpha = 0.8, ...)

layer_stars(x, alpha = 0.5, ...)

layer_delaunay(x, ...)

layer_density(
  x,
  levels_density = 20,
  levels_contour = 4,
  alpha = 1/3,
  n = 200,
  density = TRUE,
  contour = TRUE
)

layer_labelpoints(
  x,
  col = par("fg"),
  cex = 2/3,
  font = 1,
  abbreviate = FALSE,
  ...
)

layer_labelgroups(
  x,
  col = par("fg"),
  cex = 3/4,
  font = 2,
  rect = TRUE,
  alpha = 1/4,
  abbreviate = FALSE,
  ...
)

layer_rug(x, size = 1/200, ...)

layer_histogram_2(x, freq = FALSE, breaks, split = FALSE, transp = 0)

layer_density_2(x, bw, split = FALSE, rug = TRUE, transp = 0)

layer_title(x, title = "", cex = 3/4, ...)

layer_axesnames(x, cex = 3/4, name = "Axis", ...)

layer_eigen(x, nb_max = 5, cex = 1/2, ...)

layer_axesvar(x, cex = 3/4, ...)

layer_legend(x, probs = seq(0, 1, 0.25), cex = 3/4, ...)
```

## Arguments

- x:

  a list, typically returned by
  [plot_PCA](http://momx.github.io/Momocs/reference/plot_PCA.md)

- center_origin:

  `logical` whether to center the origin (default `TRUE`)

- zoom:

  `numeric` to change the zoom (default `0.9`)

- col:

  color (hexadecimal) to use for drawing components

- lwd:

  linewidth for drawing components

- ...:

  additional options to feed core functions for each layer

- cex:

  to use for drawing components

- lty:

  linetype for drawing components

- grid:

  `numeric` number of grid to draw

- border:

  color (hexadecimal) to use to draw border

- pch:

  to use for drawing components

- transp:

  transparency to use (min: 0 defaut:0 max:1)

- conf:

  `numeric` between 0 and 1 for confidence ellipses

- alpha:

  `numeric` between 0 and 1 for the transparency of components

- levels_density:

  `numeric` number of levels to use to feed
  [`MASS::kde2d`](https://rdrr.io/pkg/MASS/man/kde2d.html)

- levels_contour:

  `numeric` number of levels to use to feed
  [`graphics::contour`](https://rdrr.io/r/graphics/contour.html)

- n:

  `numeric` number of grid points to feed
  [`MASS::kde2d`](https://rdrr.io/pkg/MASS/man/kde2d.html)

- density:

  `logical` whether to draw density estimate

- contour:

  `logical` whether to draw contour lines

- font:

  to feed [text](https://rdrr.io/r/graphics/text.html)

- abbreviate:

  `logical` whether to abbreviate names

- rect:

  `logical` whether to draw a rectangle below names

- size:

  `numeric` as a fraction of graphical window (default: `1/200`)

- freq:

  `logical`to feed`[hist] (default:`FALSE\`)

- breaks:

  to feed [hist](https://rdrr.io/r/graphics/hist.html) (default:
  calculated on the pooled values)

- split:

  `logical` whether to split the two distributions into two plots

- bw:

  to feed [density](https://rdrr.io/r/stats/density.html) (default:
  stats::bw.nrd0)

- rug:

  `logical` whether to add [rug](https://rdrr.io/r/graphics/rug.html)
  (default: `TRUE`)

- title:

  to add to the plot (default `""`)

- name:

  to use on axes (default `"Axis"`)

- nb_max:

  `numeric` number of eigen values to display (default `5`)

- probs:

  `numeric` sequence to feed
  [`stats::quantile`](https://rdrr.io/r/stats/quantile.html) and to
  indicate where to draw ticks and legend labels

## Value

a drawing layer

## See also

grindr_drawers

Other grindr:
[`drawers`](http://momx.github.io/Momocs/reference/drawers.md),
[`layers_morphospace`](http://momx.github.io/Momocs/reference/layers_morphospace.md),
[`mosaic_engine()`](http://momx.github.io/Momocs/reference/mosaic.md),
[`papers`](http://momx.github.io/Momocs/reference/papers.md),
[`pile()`](http://momx.github.io/Momocs/reference/pile.md),
[`plot_LDA()`](http://momx.github.io/Momocs/reference/plot_LDA.md),
[`plot_NMDS()`](http://momx.github.io/Momocs/reference/plot_NMDS.md),
[`plot_PCA()`](http://momx.github.io/Momocs/reference/plot_PCA.md)
