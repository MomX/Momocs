# Calculates the length of a shape

Nothing more than `coo_lw(coo)[1]`.

## Usage

``` r
coo_length(coo)
```

## Arguments

- coo:

  a `matrix` of (x; y) coordinates or a Coo object

## Value

the length (in pixels) of the shape

## Details

This function can be used to integrate size - if meaningful - to Coo
objects. See also
[coo_centsize](http://momx.github.io/Momocs/reference/coo_centsize.md)
and [rescale](http://momx.github.io/Momocs/reference/rescale.md).

## See also

[coo_lw](http://momx.github.io/Momocs/reference/coo_lw.md),
[coo_width](http://momx.github.io/Momocs/reference/coo_width.md)

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
[`coo_lw()`](http://momx.github.io/Momocs/reference/coo_lw.md),
[`coo_rectangularity()`](http://momx.github.io/Momocs/reference/coo_rectangularity.md),
[`coo_rectilinearity()`](http://momx.github.io/Momocs/reference/coo_rectilinearity.md),
[`coo_scalars()`](http://momx.github.io/Momocs/reference/coo_scalars.md),
[`coo_solidity()`](http://momx.github.io/Momocs/reference/coo_solidity.md),
[`coo_tac()`](http://momx.github.io/Momocs/reference/coo_tac.md),
[`coo_width()`](http://momx.github.io/Momocs/reference/coo_width.md)

## Examples

``` r
coo_length(bot[1])
#> [1] 1087.831
coo_length(bot)
#>         brahma          caney         chimay         corona    deusventrue 
#>      1087.8309       994.1615       643.8746       805.9889       886.0715 
#>          duvel   franziskaner     grimbergen        guiness     hoegardeen 
#>       606.0107       865.0272       765.0962       742.1752      1048.1058 
#>        jupiler     kingfisher       latrappe lindemanskriek    nicechouffe 
#>       984.2303       718.3227       737.3475       821.1190       686.2766 
#>     pecheresse   sierranevada     tanglefoot          tauro      westmalle 
#>       928.6771       654.2412       680.6856       984.3941       768.0226 
#>          amrut    ballantines      bushmills         chivas        dalmore 
#>       864.0899       707.8465       882.0460       793.0187       672.0897 
#>   famousgrouse    glendronach   glenmorangie   highlandpark    jackdaniels 
#>       608.2238       822.0508       986.0991       705.9991       793.1392 
#>             jb  johnniewalker       magallan     makersmark           oban 
#>      1008.1334       337.7117       759.0041       851.3161       862.0016 
#>     oldpotrero      redbreast         tamdhu     wildturkey         yoichi 
#>       596.0958       426.0429      1008.3194      1097.1657       712.1001 
mutate(bot, size=coo_length(bot))
#> Out (outlines)
#>   - 40 outlines, 162 +/- 21 coords (in $coo)
#>   - 3 classifiers (in $fac): 
#> # A tibble: 40 × 3
#>   type   fake   size
#>   <fct>  <fct> <dbl>
#> 1 whisky a     1088.
#> 2 whisky a      994.
#> 3 whisky a      644.
#> 4 whisky a      806.
#> 5 whisky a      886.
#> 6 whisky a      606.
#> # ℹ 34 more rows
#>   - also: $ldk
```
