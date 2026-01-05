# Morphospace layers

Used internally by
[plot_PCA](http://momx.github.io/Momocs/reference/plot_PCA.md),
[plot_LDA](http://momx.github.io/Momocs/reference/plot_LDA.md), etc. but
may be useful elsewhere.

## Usage

``` r
layer_morphospace_PCA(
  x,
  position = c("range", "full", "circle", "xy", "range_axes", "full_axes")[1],
  nb = 12,
  nr = 6,
  nc = 5,
  rotate = 0,
  size = 0.9,
  col = "#999999",
  flipx = FALSE,
  flipy = FALSE,
  draw = TRUE
)

layer_morphospace_LDA(
  x,
  position = c("range", "full", "circle", "xy", "range_axes", "full_axes")[1],
  nb = 12,
  nr = 6,
  nc = 5,
  rotate = 0,
  size = 0.9,
  col = "#999999",
  flipx = FALSE,
  flipy = FALSE,
  draw = TRUE
)
```

## Arguments

- x:

  layered [PCA](http://momx.github.io/Momocs/reference/PCA.md) or
  [LDA](http://momx.github.io/Momocs/reference/LDA.md). Typically, the
  object returned by
  [plot_PCA](http://momx.github.io/Momocs/reference/plot_PCA.md) and
  [plot_LDA](http://momx.github.io/Momocs/reference/plot_LDA.md)

- position:

  one of `range, full, circle, xy, range_axes, full_axes` to feed
  [morphospace_positions](http://momx.github.io/Momocs/reference/morphospace_positions.md)
  (default: `range`)

- nb:

  `numeric` total number of shapes when `position="circle"` (default:
  `12`)

- nr:

  `numeric` number of rows to position shapes (default: `6`)

- nc:

  `numeric` number of columns to position shapes (default `5`)

- rotate:

  `numeric` angle (in radians) to rotate shapes when displayed on the
  morphospace (default: `0`)

- size:

  `numeric` size to use to feed
  [coo_template](http://momx.github.io/Momocs/reference/coo_template.md)
  (default: `0.9`)

- col:

  color to draw shapes (default: `#999999`)

- flipx:

  `logical` whether to flip shapes against the x-axis (default: `FALSE`)

- flipy:

  `logical` whether to flip shapes against the y-axis (default: `FALSE`)

- draw:

  `logical` whether to draw shapes (default: `TRUE`)

## Value

a drawing layer

## See also

Other grindr:
[`drawers`](http://momx.github.io/Momocs/reference/drawers.md),
[`layers`](http://momx.github.io/Momocs/reference/layers.md),
[`mosaic_engine()`](http://momx.github.io/Momocs/reference/mosaic.md),
[`papers`](http://momx.github.io/Momocs/reference/papers.md),
[`pile()`](http://momx.github.io/Momocs/reference/pile.md),
[`plot_LDA()`](http://momx.github.io/Momocs/reference/plot_LDA.md),
[`plot_NMDS()`](http://momx.github.io/Momocs/reference/plot_NMDS.md),
[`plot_PCA()`](http://momx.github.io/Momocs/reference/plot_PCA.md)

Other grindr:
[`drawers`](http://momx.github.io/Momocs/reference/drawers.md),
[`layers`](http://momx.github.io/Momocs/reference/layers.md),
[`mosaic_engine()`](http://momx.github.io/Momocs/reference/mosaic.md),
[`papers`](http://momx.github.io/Momocs/reference/papers.md),
[`pile()`](http://momx.github.io/Momocs/reference/pile.md),
[`plot_LDA()`](http://momx.github.io/Momocs/reference/plot_LDA.md),
[`plot_NMDS()`](http://momx.github.io/Momocs/reference/plot_NMDS.md),
[`plot_PCA()`](http://momx.github.io/Momocs/reference/plot_PCA.md)
