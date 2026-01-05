# Checks shapes

A simple utility, used internally, mostly in the coo functions and
methods. Returns a matrix of coordinates, when passed with either a list
or a `matrix` of coordinates.

## Usage

``` r
coo_check(coo)
```

## Arguments

- coo:

  `matrix` of `(x; y)` coordinates or any
  [Coo](http://momx.github.io/Momocs/reference/Coo.md) object.

## Value

`matrix` of `(x; y)` coordinates or a
[Coo](http://momx.github.io/Momocs/reference/Coo.md) object.

## Examples

``` r
#coo_check('Not a shape')
#coo_check(iris)
#coo_check(matrix(1:10, ncol=2))
#coo_check(list(x=1:5, y=6:10))
```
