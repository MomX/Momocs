# Adds new landmarks on Out and Opn objects

Helps to add new landmarks on a `Coo` object on top of existing ones.
The number of landmarks must be specified and rows indices that
correspond to the nearest points clicked on every outlines are stored in
the `$ldk` slot of the `Coo` object.

## Usage

``` r
add_ldk(Coo, nb.ldk)
```

## Arguments

- Coo:

  an Out or Opn object

- nb.ldk:

  the number of landmarks to add on every shape

## Value

an Out or an Opn object with some landmarks defined

## Details

Note that if no landmarks are already defined, then this function is
equivalent to
[def_ldk](http://momx.github.io/Momocs/reference/def_ldk.md).

## See also

Other ldk/slidings methods:
[`def_ldk()`](http://momx.github.io/Momocs/reference/def_ldk.md),
[`def_slidings()`](http://momx.github.io/Momocs/reference/def_slidings.md),
[`get_ldk()`](http://momx.github.io/Momocs/reference/get_ldk.md),
[`get_slidings()`](http://momx.github.io/Momocs/reference/get_slidings.md),
[`rearrange_ldk()`](http://momx.github.io/Momocs/reference/rearrange_ldk.md),
[`slidings_scheme()`](http://momx.github.io/Momocs/reference/slidings_scheme.md)

## Examples

``` r
if (FALSE) { # \dontrun{
hearts <- slice(hearts, 1:5) # to make it shorter to try
# click on 3 points, 5 times.
hearts <- def_ldk(hearts, 3)
# Don't forget to save the object returned by def_ldk...
hearts2 <- add_ldk(hearts, 3)
stack(hearts2)
hearts2$ldk
} # }
```
