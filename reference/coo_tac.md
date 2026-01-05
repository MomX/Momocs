# Calculates the total absolute curvature of a shape

Calculated using the sum of the absolute value of the second derivative
of the `smooth.spline` prediction for each defined point.

## Usage

``` r
coo_tac(coo)
```

## Source

Siobhan Braybrook.

## Arguments

- coo:

  a `matrix` of (x; y) coordinates or any `Coo`

## Value

`numeric` for a single shape and for `Coo`

## See also

Other coo\_ descriptors:
[`coo_angle_edges()`](http://momx.github.io/Momocs/reference/coo_angle_edges.md),
[`coo_angle_tangent()`](http://momx.github.io/Momocs/reference/coo_angle_tangent.md),
[`coo_area()`](http://momx.github.io/Momocs/reference/coo_area.md),
[`coo_boundingbox()`](http://momx.github.io/Momocs/reference/coo_boundingbox.md),
[`coo_chull()`](http://momx.github.io/Momocs/reference/coo_chull.md),
[`coo_circularity()`](http://momx.github.io/Momocs/reference/coo_circularity.md),
[`coo_convexity()`](http://momx.github.io/Momocs/reference/coo_convexity.md),
[`coo_eccentricity`](http://momx.github.io/Momocs/reference/coo_eccentricity.md),
[`coo_elongation()`](http://momx.github.io/Momocs/reference/coo_elongation.md),
[`coo_length()`](http://momx.github.io/Momocs/reference/coo_length.md),
[`coo_lw()`](http://momx.github.io/Momocs/reference/coo_lw.md),
[`coo_rectangularity()`](http://momx.github.io/Momocs/reference/coo_rectangularity.md),
[`coo_rectilinearity()`](http://momx.github.io/Momocs/reference/coo_rectilinearity.md),
[`coo_scalars()`](http://momx.github.io/Momocs/reference/coo_scalars.md),
[`coo_solidity()`](http://momx.github.io/Momocs/reference/coo_solidity.md),
[`coo_width()`](http://momx.github.io/Momocs/reference/coo_width.md)

## Examples

``` r
coo_tac(bot[1])
#> [1] 67.85005

bot %>%
    slice(1:3) %>%  # for speed sake only
    coo_tac
#>   brahma    caney   chimay 
#> 67.85005 34.81994 46.61054 
```
