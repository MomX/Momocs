# Checks 'ldk' shapes

A simple utility, used internally, mostly by
[Ldk](http://momx.github.io/Momocs/reference/Ldk.md) methods, in some
graphical functions, and notably in
[l2a](http://momx.github.io/Momocs/reference/bridges.md). Returns an
array of landmarks arranged as `(nb.ldk) x (x; y) x (nb.shapes)`, when
passed with either a list, a matrix or an array of coordinates. If a
list is provided, checks that the number of landmarks is consistent.

## Usage

``` r
ldk_check(ldk)
```

## Arguments

- ldk:

  a `matrix` of (x; y) coordinates, a list, or an array.

## Value

an `array` of (x; y) coordinates.

## See also

Other ldk helpers:
[`def_links()`](http://momx.github.io/Momocs/reference/def_links.md),
[`links_all()`](http://momx.github.io/Momocs/reference/links_all.md),
[`links_delaunay()`](http://momx.github.io/Momocs/reference/links_delaunay.md)

## Examples

``` r
#coo_check('Not a shape')
#coo_check(matrix(1:10, ncol=2))
#coo_check(list(x=1:5, y=6:10))
```
