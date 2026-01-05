# Defines new landmarks on Out and Opn objects

Helps to define landmarks on a `Coo` object. The number of landmarks
must be specified and rows indices that correspond to the nearest points
clicked on every outlines are stored in the `$ldk` slot of the `Coo`
object.

## Usage

``` r
def_ldk(Coo, nb.ldk, close, points)
```

## Arguments

- Coo:

  an Out or Opn object

- nb.ldk:

  the number of landmarks to define on every shape

- close:

  `logical` whether to close (typically for outlines)

- points:

  `logical` whether to display points

## Value

an Out or an Opn object with some landmarks defined

## See also

Other ldk/slidings methods:
[`add_ldk()`](http://momx.github.io/Momocs/reference/add_ldk.md),
[`def_slidings()`](http://momx.github.io/Momocs/reference/def_slidings.md),
[`get_ldk()`](http://momx.github.io/Momocs/reference/get_ldk.md),
[`get_slidings()`](http://momx.github.io/Momocs/reference/get_slidings.md),
[`rearrange_ldk()`](http://momx.github.io/Momocs/reference/rearrange_ldk.md),
[`slidings_scheme()`](http://momx.github.io/Momocs/reference/slidings_scheme.md)

## Examples

``` r
if (FALSE) { # \dontrun{
bot <- bot[1:5] # to make it shorter to try
# click on 3 points, 5 times.
# Don't forget to save the object returned by def_ldk...
bot2 <- def_ldk(bot, 3)
stack(bot2)
bot2$ldk
} # }
```
