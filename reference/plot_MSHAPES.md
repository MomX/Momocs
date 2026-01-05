# Pairwise comparison of a list of shapes

"Confusion matrix" of a list of shapes. See examples.

## Usage

``` r
plot_MSHAPES(x, draw_fun, size, palette)
```

## Arguments

- x:

  a list of shapes (eg as returned by
  [MSHAPES](http://momx.github.io/Momocs/reference/MSHAPES.md))

- draw_fun:

  one of
  [draw_outline](http://momx.github.io/Momocs/reference/drawers.md),
  [draw_curves](http://momx.github.io/Momocs/reference/drawers.md),
  [draw_landmarks](http://momx.github.io/Momocs/reference/drawers.md).
  When the result of
  [MSHAPES](http://momx.github.io/Momocs/reference/MSHAPES.md) is
  passed, detected based on `$Coe`, otherwise default to `draw_curves`.

- size:

  numeric shrinking factor for shapes (and
  [coo_template](http://momx.github.io/Momocs/reference/coo_template.md);
  3/4 by default)

- palette:

  on of [palettes](http://momx.github.io/Momocs/reference/palettes.md)

## Value

a plot

## Note

Directly inspired by Chitwood et al. (2016) in *New Phytologist*

## Examples

``` r
x <- bot %>% efourier(6) %>% MSHAPES(~type)
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details

# custom colors
x %>% plot_MSHAPES(palette=pal_manual(c("darkgreen", "orange")))


# also works on list of shapes, eg:
leaves <- shapes %>% slice(grep("leaf", names(shapes))) %$% coo
class(leaves)
#> [1] "list"
leaves %>% plot_MSHAPES()


# or
shapes %>%
# subset and degrade
slice(1:12) %>% coo_sample(60) %$%  # grab the coo
    coo %>%
    plot_MSHAPES()
```
