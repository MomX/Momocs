# Calculates euclidean distance between two points.

`ed` simply calculates euclidean distance between two points defined by
their (x; y) coordinates.

## Usage

``` r
ed(pt1, pt2)
```

## Arguments

- pt1:

  (x; y) coordinates of the first point.

- pt2:

  (x; y) coordinates of the second point.

## Value

Returns the euclidean distance between the two points.

## See also

[edm](http://momx.github.io/Momocs/reference/edm.md),
[edm_nearest](http://momx.github.io/Momocs/reference/edm_nearest.md),
[dist](https://rdrr.io/r/stats/dist.html).

## Examples

``` r
ed(c(0,1), c(1,0))
#> [1] 1.414214
```
