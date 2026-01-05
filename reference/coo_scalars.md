# Calculates all scalar descriptors of shape

See examples for the full list.

## Usage

``` r
coo_scalars(coo, rectilinearity = FALSE)
```

## Arguments

- coo:

  a `matrix` of (x; y) coordinates or any `Coo`

- rectilinearity:

  `logical` whether to include rectilinearity using
  [coo_rectilinearity](http://momx.github.io/Momocs/reference/coo_rectilinearity.md)

## Value

`data_frame`

## Details

[coo_rectilinearity](http://momx.github.io/Momocs/reference/coo_rectilinearity.md)
being not particularly optimized, it takes around 30 times more time to
include it than to calculate *all* others and is thus not includedby
default. by default.

## See also

Other coo\_ descriptors:
[`coo_angle_edges()`](http://momx.github.io/Momocs/reference/coo_angle_edges.md),
[`coo_angle_tangent()`](http://momx.github.io/Momocs/reference/coo_angle_tangent.md),
[`coo_area()`](http://momx.github.io/Momocs/reference/coo_area.md),
[`coo_boundingbox()`](http://momx.github.io/Momocs/reference/coo_boundingbox.md),
[`coo_chull()`](http://momx.github.io/Momocs/reference/coo_chull.md),
[`coo_circularity()`](http://momx.github.io/Momocs/reference/coo_circularity.md),
[`coo_convexity()`](http://momx.github.io/Momocs/reference/coo_convexity.md),
[`coo_eccentricity`](http://momx.github.io/Momocs/reference/coo_eccentricity.md),
[`coo_elongation()`](http://momx.github.io/Momocs/reference/coo_elongation.md),
[`coo_length()`](http://momx.github.io/Momocs/reference/coo_length.md),
[`coo_lw()`](http://momx.github.io/Momocs/reference/coo_lw.md),
[`coo_rectangularity()`](http://momx.github.io/Momocs/reference/coo_rectangularity.md),
[`coo_rectilinearity()`](http://momx.github.io/Momocs/reference/coo_rectilinearity.md),
[`coo_solidity()`](http://momx.github.io/Momocs/reference/coo_solidity.md),
[`coo_tac()`](http://momx.github.io/Momocs/reference/coo_tac.md),
[`coo_width()`](http://momx.github.io/Momocs/reference/coo_width.md)

## Examples

``` r
df <- bot %>% coo_scalars() # pass bot %>% coo_scalars(TRUE) if you want rectilinearity
colnames(df) %>% cat(sep="\n") # all scalars used
#> area
#> calliper
#> centsize
#> circularity
#> circularityharalick
#> circularitynorm
#> convexity
#> eccentricityboundingbox
#> eccentricityeigen
#> elongation
#> length
#> perim
#> rectangularity
#> solidity
#> width

# a PCA on all these descriptors
TraCoe(coo_scalars(bot), fac=bot$fac) %>% PCA %>% plot_PCA(~type)

```
