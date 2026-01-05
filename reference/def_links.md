# Defines links between landmarks

Works on Ldk objects, on 2-cols matrices, 3-dim arrays
([MSHAPES](http://momx.github.io/Momocs/reference/MSHAPES.md) turns it
into a matrix).

## Usage

``` r
def_links(x, nb.ldk)
```

## Arguments

- x:

  Ldk, matric or array

- nb.ldk:

  numeric the iterative procedure is stopped when the user click on the
  top of the graphical window.

## Value

a Momocs object of same class

## See also

Other ldk helpers:
[`ldk_check()`](http://momx.github.io/Momocs/reference/ldk_check.md),
[`links_all()`](http://momx.github.io/Momocs/reference/links_all.md),
[`links_delaunay()`](http://momx.github.io/Momocs/reference/links_delaunay.md)

## Examples

``` r
if (FALSE) { # \dontrun{
wm <- MSHAPES(wings)
links <- def_links(wm, 3) # click to define pairs of landmarks
ldk_links(wm, links)
} # }
```
