# Calculates convex hull area/volume of PCA scores

May be useful to compare shape diversity. Expressed in PCA units that
should only be compared within the same PCA.

## Usage

``` r
get_chull_area(x, fac, xax = 1, yax = 2)

get_chull_volume(x, fac, xax = 1, yax = 2, zax = 3)
```

## Arguments

- x:

  a PCA object

- fac:

  (optionnal) column name or ID from the \$fac slot.

- xax:

  the first PC axis to use (1 by default)

- yax:

  the second PC axis (2 by default)

- zax:

  the third PC axis (3 by default only for volume)

## Value

If fac is not provided global area/volume is returned; otherwise a named
list for every level of fac

## Details

get_chull_area is calculated using
[coo_chull](http://momx.github.io/Momocs/reference/coo_chull.md)
followed by
[coo_area](http://momx.github.io/Momocs/reference/coo_area.md);
get_chull_volume is calculated using geometry::convexhulln

## Examples

``` r
bp <- PCA(efourier(bot, 12))
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details
get_chull_area(bp)
#> [1] 0.01968577
get_chull_area(bp, 1)
#> $beer
#> [1] 0.01802331
#> 
#> $whisky
#> [1] 0.008768242
#> 

get_chull_volume(bp)
#> [1] 0.0005563784
get_chull_volume(bp, 1)
#> $beer
#> [1] 0.0004506466
#> 
#> $whisky
#> [1] 0.0001181342
#> 
```
