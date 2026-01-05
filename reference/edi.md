# Calculates euclidean intermediate between two points.

`edi` simply calculates coordinates of a points at the relative distance
`r` on the `pt1-pt2` defined by their (x; y) coordinates. This function
is used internally but may be of interest for other analyses.

## Usage

``` r
edi(pt1, pt2, r = 0.5)
```

## Arguments

- pt1:

  \\(x; y)\\ coordinates of the first point.

- pt2:

  \\(x; y)\\ coordinates of the second point.

- r:

  the relative distance from `pt1` to `pt2`.

## Value

returns the \\(x; y)\\ interpolated coordinates.

## See also

[ed](http://momx.github.io/Momocs/reference/ed.md),
[edm](http://momx.github.io/Momocs/reference/edm.md).

## Examples

``` r
edi(c(0,1), c(1,0), r = 0.5)
#> [1] 0.5 0.5
```
