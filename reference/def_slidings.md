# Defines sliding landmarks matrix

Defines sliding landmarks matrix

## Usage

``` r
def_slidings(Coo, slidings)
```

## Arguments

- Coo:

  an [Ldk](http://momx.github.io/Momocs/reference/Ldk.md) object

- slidings:

  a matrix, a numeric or a list of numeric. See Details

## Value

a Momocs object of same class

## Details

`$slidings` in [Ldk](http://momx.github.io/Momocs/reference/Ldk.md) must
be a 'valid' matrix: containing ids of coordinates, none of them being
lower than 1 and higher the number of coordinates in `$coo`.

`slidings` matrix contains 3 columns (`before`, `slide`, `after`). It is
inspired by `geomorph` and should be compatible with it.

This matrix can be passed directly if the `slidings` argument is a
matrix. Of course, it is strictly equivalent to
`Ldk$slidings <- slidings`.

`slidings` can also be passed as "partition(s)", when sliding landmarks
identified by their ids (which are a row number) are consecutive in the
`$coo`.

A single partition can be passed either as a numeric (eg `4:12`), if
points 5 to 11 must be considered as sliding landmarks (4 and 12 being
fixed); or as a list of numeric.

See examples below.

## See also

Other ldk/slidings methods:
[`add_ldk()`](http://momx.github.io/Momocs/reference/add_ldk.md),
[`def_ldk()`](http://momx.github.io/Momocs/reference/def_ldk.md),
[`get_ldk()`](http://momx.github.io/Momocs/reference/get_ldk.md),
[`get_slidings()`](http://momx.github.io/Momocs/reference/get_slidings.md),
[`rearrange_ldk()`](http://momx.github.io/Momocs/reference/rearrange_ldk.md),
[`slidings_scheme()`](http://momx.github.io/Momocs/reference/slidings_scheme.md)

## Examples

``` r
#waiting for a sliding dataset...
```
