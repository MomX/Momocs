# Calculates the angle of every edge of a shape

Returns the angle (in radians) of every edge of a shape,

## Usage

``` r
coo_angle_edges(coo, method = c("atan2", "acos")[1])

# Default S3 method
coo_angle_edges(coo, method = c("atan2", "acos")[1])

# S3 method for class 'Coo'
coo_angle_edges(coo, method = c("atan2", "acos")[1])
```

## Arguments

- coo:

  a `matrix` or a list of (x; y) coordinates or any `Coo`

- method:

  'atan2' (or 'acos') for a signed (or not) angle.

## Value

`numeric` the angles in radians for every edge.

## Note

`coo_thetapts` is deprecated and will be removed in future releases.

## See also

Other coo\_ descriptors:
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
[`coo_tac()`](http://momx.github.io/Momocs/reference/coo_tac.md),
[`coo_width()`](http://momx.github.io/Momocs/reference/coo_width.md)

## Examples

``` r
b <- coo_sample(bot[1], 64)
coo_angle_edges(b)
#>  [1] -3.111523 -3.093172 -3.092305  3.081724  3.105229  3.077881  3.110353
#>  [8]  3.046641  3.051196  3.135756 -3.074763 -3.017238 -2.926235 -2.628633
#> [15] -2.611156 -2.953906 -3.017238 -3.085049 -2.915581 -2.709056 -2.530867
#> [22] -2.927854 -3.086548 -3.100971  3.114884 -3.060821  3.116579 -3.106746
#> [29]  3.131759  3.116579  3.080363  3.030147  3.035444  3.141593 -3.051196
#> [36] -3.014394 -2.988492 -2.873097 -3.131789  2.993915  2.944112 -3.135728
#> [43]  3.113711  3.137875 -3.106026  3.106746  2.980423 -1.981456 -2.694480
#> [50] -3.062770 -2.583631 -2.029263  3.018319  3.023726 -3.109950  3.085522
#> [57] -3.087530  3.112543  2.978918  3.011861  3.087633 -2.957901 -2.916681
#> [64] -3.046946
```
