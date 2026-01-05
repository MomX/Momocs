# Draw radii to the current plot

Given a shape, all centroid-points radii are drawn using
[segments](https://rdrr.io/r/graphics/segments.html) that can be passed
with options

## Usage

``` r
coo_draw_rads(coo, ...)
```

## Arguments

- coo:

  a shape

- ...:

  arguments to feed [segments](https://rdrr.io/r/graphics/segments.html)

## Value

a drawing on the last plot

## Examples

``` r
shp <- shapes[4] %>% coo_sample(24) %T>% coo_plot
coo_draw_rads(shp, col=col_summer(24))

```
