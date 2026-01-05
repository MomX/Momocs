# Creates links (Delaunay triangulation) between landmarks

Creates links (Delaunay triangulation) between landmarks

## Usage

``` r
links_delaunay(coo)
```

## Arguments

- coo:

  a matrix (or a list) of (x; y) coordinates

## Value

a matrix that can be passed to
[ldk_links](http://momx.github.io/Momocs/reference/ldk_links.md), etc.
The columns are the row ids of the original shape.

## Details

uses delaunayn in the `geometry` package.

## See also

Other ldk helpers:
[`def_links()`](http://momx.github.io/Momocs/reference/def_links.md),
[`ldk_check()`](http://momx.github.io/Momocs/reference/ldk_check.md),
[`links_all()`](http://momx.github.io/Momocs/reference/links_all.md)

## Examples

``` r
w <- wings[1]
coo_plot(w, poly=FALSE)
links <- links_delaunay(w)
ldk_links(w, links)
```
