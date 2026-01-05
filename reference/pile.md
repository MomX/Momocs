# Graphical pile of shapes

Pile all shapes in the same graphical window. Useful to check their
normalization in terms of size, position, rotation, first point, etc. It
is, essentially, a shortcut around `paper + drawers` of the grindr
family.

## Usage

``` r
pile(coo, f, sample, subset, pal, paper_fun, draw_fun, transp, ...)

# Default S3 method
pile(
  coo,
  f,
  sample,
  subset,
  pal = pal_qual,
  paper_fun = paper,
  draw_fun = draw_curves,
  transp = 0,
  ...
)

# S3 method for class 'list'
pile(
  coo,
  f,
  sample = 64,
  subset = 1000,
  pal = pal_qual,
  paper_fun = paper,
  draw_fun = draw_curves,
  transp = 0,
  ...
)

# S3 method for class 'array'
pile(
  coo,
  f,
  sample = 64,
  subset = 1000,
  pal = pal_qual,
  paper_fun = paper,
  draw_fun = draw_landmarks,
  transp = 0,
  ...
)

# S3 method for class 'Out'
pile(
  coo,
  f,
  sample = 64,
  subset = 1000,
  pal = pal_qual,
  paper_fun = paper,
  draw_fun = draw_outlines,
  transp = 0,
  ...
)

# S3 method for class 'Opn'
pile(
  coo,
  f,
  sample = 64,
  subset = 1000,
  pal = pal_qual,
  paper_fun = paper,
  draw_fun = draw_curves,
  transp = 0,
  ...
)

# S3 method for class 'Ldk'
pile(
  coo,
  f,
  sample = 64,
  subset = 1000,
  pal = pal_qual,
  paper_fun = paper,
  draw_fun = draw_landmarks,
  transp = 0,
  ...
)
```

## Arguments

- coo:

  a single shape or any
  [Coo](http://momx.github.io/Momocs/reference/Coo.md) object

- f:

  factor specification

- sample:

  `numeric` number of points to
  [coo_sample](http://momx.github.io/Momocs/reference/coo_sample.md) if
  the number of shapes is \> 1000 (default: 64). If non-numeric (eg
  `FALSE`) do not sample.

- subset:

  `numeric` only draw this number of (randomly chosen) shapes if the
  number of shapes is \> 1000 (default: 1000) If non-numeric (eg
  `FALSE`) do not sample.

- pal:

  palette among
  [palettes](http://momx.github.io/Momocs/reference/palettes.md)
  (default: pal_qual)

- paper_fun:

  a [papers](http://momx.github.io/Momocs/reference/papers.md) function
  (default: `paper`)

- draw_fun:

  one of [drawers](http://momx.github.io/Momocs/reference/drawers.md)
  for `pile.list`

- transp:

  `numeric` for transparency (default:adjusted, min:0, max=0)

- ...:

  more arguments to feed the core drawer, depending on the object

## Value

a plot

## Details

Large `Coo` are sampled, both in terms of the number of shapes and of
points to drawn.

## Note

A variation of this plot was called `stack` before `Momocs 1.2.5`

## See also

Other grindr:
[`drawers`](http://momx.github.io/Momocs/reference/drawers.md),
[`layers`](http://momx.github.io/Momocs/reference/layers.md),
[`layers_morphospace`](http://momx.github.io/Momocs/reference/layers_morphospace.md),
[`mosaic_engine()`](http://momx.github.io/Momocs/reference/mosaic.md),
[`papers`](http://momx.github.io/Momocs/reference/papers.md),
[`plot_LDA()`](http://momx.github.io/Momocs/reference/plot_LDA.md),
[`plot_NMDS()`](http://momx.github.io/Momocs/reference/plot_NMDS.md),
[`plot_PCA()`](http://momx.github.io/Momocs/reference/plot_PCA.md)

## Examples

``` r
# all Coo are supported with sensible defaults
pile(bot)    # outlines

pile(olea, ~var, pal=pal_qual_Dark2, paper_fun=paper_grid)   # curves

pile(wings)  # landmarks


# you can continue the pipe with compatible drawers
pile(bot, trans=0.9) %>% draw_centroid


# if you are not happy with this, build your own !
# eg see Momocs::pile.Out (no quotes)

my_pile <- function(x, col_labels="red", transp=0.5){
    x %>% paper_chess(n=100) %>%
          draw_landmarks(transp=transp) %>%
          draw_labels(col=col_labels)
}
# using it
wings %>% my_pile(transp=3/4)


 # and as gridr functions propagate, you can even continue:
 wings %>% my_pile() %>% draw_centroid(col="blue", cex=5)


 # method on lists
 bot$coo %>% pile


 # it can be tuned when we have a list of landmarks with:
 wings$coo %>% pile(draw_fun=draw_landmarks)


 # or on arrays (turn for draw_landmarks)
 wings$coo %>% l2a %>% #we now have an array
     pile
```
