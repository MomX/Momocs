# Plots mosaics of shapes.

Will soon replace
[panel](http://momx.github.io/Momocs/reference/panel.Coo.md). See
examples and vignettes.

## Usage

``` r
mosaic_engine(
  coo_list,
  dim,
  asp = 1,
  byrow = TRUE,
  fromtop = TRUE,
  sample = 60,
  relatively = FALSE,
  template_size = 0.92
)

mosaic(x, ...)

# S3 method for class 'Out'
mosaic(
  x,
  f,
  relatively = FALSE,
  pal = pal_qual,
  sample = 60,
  paper_fun = paper_white,
  draw_fun = draw_outlines,
  legend = TRUE,
  dim = NA,
  asp = 1,
  byrow = TRUE,
  fromtop = TRUE,
  ...
)

# S3 method for class 'Opn'
mosaic(
  x,
  f,
  relatively = FALSE,
  pal = pal_qual,
  sample = 60,
  paper_fun = paper_white,
  draw_fun = draw_curves,
  legend = TRUE,
  dim = NA,
  asp = 1,
  byrow = TRUE,
  fromtop = TRUE,
  ...
)

# S3 method for class 'Ldk'
mosaic(
  x,
  f,
  relatively = FALSE,
  pal = pal_qual,
  sample = 60,
  paper_fun = paper_white,
  draw_fun = draw_landmarks,
  legend = TRUE,
  dim = NA,
  asp = 1,
  byrow = TRUE,
  fromtop = TRUE,
  ...
)
```

## Arguments

- coo_list:

  `list` of shapes

- dim:

  `numeric` of length 2, the desired dimensions for rows and columns

- asp:

  `numeric` the yx ratio used to calculate `dim` (1 by default).

- byrow:

  `logical` whether to order shapes by rows

- fromtop:

  `logical` whether to order shapes from top

- sample:

  `numeric` number of points to
  [coo_sample](http://momx.github.io/Momocs/reference/coo_sample.md)

- relatively:

  `logical` if `TRUE` use
  [coo_template_relatively](http://momx.github.io/Momocs/reference/coo_template.md)
  or, if `FALSE`(by default)
  [coo_template](http://momx.github.io/Momocs/reference/coo_template.md).
  In other words, whether to preserve size or not.

- template_size:

  `numeric` to feed `coo_template(_relatively)`. Only useful to add
  padding around shapes when the default value (0.95) is lowered.

- x:

  any [Coo](http://momx.github.io/Momocs/reference/Coo.md) object

- ...:

  additional arguments to feed the main drawer if the number of shapes
  is \> 1000 (default: 64). If non-numeric (eg `FALSE`) do not sample.

- f:

  factor specification to feed
  [fac_dispatcher](http://momx.github.io/Momocs/reference/fac_dispatcher.md)

- pal:

  one of [palettes](http://momx.github.io/Momocs/reference/palettes.md)

- paper_fun:

  a [papers](http://momx.github.io/Momocs/reference/papers.md) function
  (default: `paper`)

- draw_fun:

  one of [drawers](http://momx.github.io/Momocs/reference/drawers.md)
  for `pile.list`

- legend:

  `logical` whether to draw a legend (will be improved in further
  versions)

## Value

a list of templated and translated shapes

## See also

Other grindr:
[`drawers`](http://momx.github.io/Momocs/reference/drawers.md),
[`layers`](http://momx.github.io/Momocs/reference/layers.md),
[`layers_morphospace`](http://momx.github.io/Momocs/reference/layers_morphospace.md),
[`papers`](http://momx.github.io/Momocs/reference/papers.md),
[`pile()`](http://momx.github.io/Momocs/reference/pile.md),
[`plot_LDA()`](http://momx.github.io/Momocs/reference/plot_LDA.md),
[`plot_NMDS()`](http://momx.github.io/Momocs/reference/plot_NMDS.md),
[`plot_PCA()`](http://momx.github.io/Momocs/reference/plot_PCA.md)

## Examples

``` r
# On Out ---
bot %>% mosaic

bot %>% mosaic(~type)


# As with other grindr functions you can continue the pipe
bot %>% mosaic(~type, asp=0.5) %>% draw_firstpoint


# On Opn ---- same grammar
olea %>% mosaic(~view+var, paper_fun=paper_dots)


 # On Ldk
 mosaic(wings, ~group, pal=pal_qual_Dark2, pch=3)


 # On Out with different sizes
 # would work on other Coo too
shapes2 <- shapes
sizes <- runif(30, 1, 2)
shapes2 %>% mosaic(relatively=FALSE)

shapes2 %>% mosaic(relatively=TRUE) %>% draw_centroid()
```
