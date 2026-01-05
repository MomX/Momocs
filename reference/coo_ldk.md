# Defines landmarks interactively

Allows to interactively define a `nb.ldk` number of landarks on a shape.
Used in other facilities to acquire/manipulate data.

## Usage

``` r
coo_ldk(coo, nb.ldk, close = FALSE, points = TRUE)
```

## Arguments

- coo:

  a `matrix` or a list of (x; y) coordinates.

- nb.ldk:

  `integer`, the number of landmarks to define

- close:

  `logical` whether to close (typically for outlines)

- points:

  `logical` whether to display points

## Value

`numeric` that corresponds to the closest ids, on the shape, from cliked
points.

## Examples

``` r
if (FALSE) { # \dontrun{
b <- bot[1]
coo_ldk(b, 3) # run this, and click 3 times
coo_ldk(bot, 2) # this also works on Out
} # }
```
