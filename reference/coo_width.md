# Calculates the width of a shape

Nothing more than `coo_lw(coo)[2]`.

## Usage

``` r
coo_width(coo)
```

## Arguments

- coo:

  a `matrix` of (x; y) coordinates or Coo object

## Value

the width (in pixels) of the shape

## See also

[coo_lw](http://momx.github.io/Momocs/reference/coo_lw.md),
[coo_length](http://momx.github.io/Momocs/reference/coo_length.md).

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
[`coo_tac()`](http://momx.github.io/Momocs/reference/coo_tac.md)

## Examples

``` r
coo_width(bot[1])
#> [1] 278.0386
```
