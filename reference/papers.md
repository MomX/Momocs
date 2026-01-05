# grindr papers for shape plots

Papers on which to use
[drawers](http://momx.github.io/Momocs/reference/drawers.md) for
building custom shape plots using the grindr approach. See examples and
vignettes.

## Usage

``` r
paper(coo, ...)

paper_white(coo)

paper_grid(coo, grid = c(10, 5), cols = c("#ffa500", "#e5e5e5"), ...)

paper_chess(coo, n = 50, col = "#E5E5E5")

paper_dots(coo, pch = 20, n = 50, col = "#7F7F7F")
```

## Arguments

- coo:

  a single shape or any
  [Coo](http://momx.github.io/Momocs/reference/Coo.md) object

- ...:

  more arguments to feed the plotting function within each `paper`
  function

- grid:

  `numeric` of length 2 to (roughly) specify the number of majors lines,
  and the number of minor lines within two major ones

- cols:

  colors (hexadecimal preferred) to use for grid drawing

- n:

  `numeric` number of squares for the chessboard

- col:

  color (hexadecimal) to use for chessboard drawing

- pch:

  to use for dots

## Value

a drawing layer

## Note

This approach will (soon) replace
[coo_plot](http://momx.github.io/Momocs/reference/coo_plot.md) and
friends in further versions. All comments are welcome.

## See also

Other grindr:
[`drawers`](http://momx.github.io/Momocs/reference/drawers.md),
[`layers`](http://momx.github.io/Momocs/reference/layers.md),
[`layers_morphospace`](http://momx.github.io/Momocs/reference/layers_morphospace.md),
[`mosaic_engine()`](http://momx.github.io/Momocs/reference/mosaic.md),
[`pile()`](http://momx.github.io/Momocs/reference/pile.md),
[`plot_LDA()`](http://momx.github.io/Momocs/reference/plot_LDA.md),
[`plot_NMDS()`](http://momx.github.io/Momocs/reference/plot_NMDS.md),
[`plot_PCA()`](http://momx.github.io/Momocs/reference/plot_PCA.md)
