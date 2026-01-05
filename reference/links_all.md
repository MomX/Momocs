# Creates links (all pairwise combinations) between landmarks

Creates links (all pairwise combinations) between landmarks

## Usage

``` r
links_all(coo)
```

## Arguments

- coo:

  a matrix (or a list) of (x; y) coordinates

## Value

a matrix that can be passed to
[ldk_links](http://momx.github.io/Momocs/reference/ldk_links.md), etc.
The columns are the row ids of the original shape.

## See also

Other ldk helpers:
[`def_links()`](http://momx.github.io/Momocs/reference/def_links.md),
[`ldk_check()`](http://momx.github.io/Momocs/reference/ldk_check.md),
[`links_delaunay()`](http://momx.github.io/Momocs/reference/links_delaunay.md)

## Examples

``` r
w <- wings[1]
coo_plot(w)
links <- links_all(w)
ldk_links(w, links)
```
