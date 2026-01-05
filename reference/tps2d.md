# Thin Plate Splines for 2D data

`tps2d` is the core function for Thin Plate Splines. It is used
internally for all TPS graphical functions.`tps_apply` is the very same
function but with arguments properly named (I maintain tps2d as it is
for historical reasons) when we want a apply a trasnformation grid.

## Usage

``` r
tps2d(grid0, fr, to)

tps_apply(fr, to, new)
```

## Arguments

- grid0:

  a matrix of coordinates on which to calculate deformations

- fr:

  the reference shape

- to:

  the target shape

- new:

  the shape on which to apply the `shp1->shp2` calibrated tps
  trasnformation

## Value

a shape.

## See also

Other thin plate splines:
[`tps_arr()`](http://momx.github.io/Momocs/reference/tps_arr.md),
[`tps_grid()`](http://momx.github.io/Momocs/reference/tps_grid.md),
[`tps_iso()`](http://momx.github.io/Momocs/reference/tps_iso.md),
[`tps_raw()`](http://momx.github.io/Momocs/reference/tps_raw.md)

## Examples

``` r
shapes <- shapes %>%
 coo_scale() %>% coo_center() %>%
 coo_slidedirection("up") %>%
 coo_sample(64)

leaf1 <- shapes[14]
leaf2 <- shapes[15]

# tps grid on the two leafs2
tps_grid(leaf1, leaf2)
# apply the (leaf1 -> leaf2) tps trasnformation  onto leaf1
# (that thus get closer to leaf2)
tps_apply(leaf1, leaf2, leaf1) %>% coo_draw(bor="purple")

```
