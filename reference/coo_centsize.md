# Calculates centroid size

Calculates centroid size

## Usage

``` r
coo_centsize(coo)
```

## Arguments

- coo:

  `matrix` of `(x; y)` coordinates or any
  [Coo](http://momx.github.io/Momocs/reference/Coo.md) object.

## Value

`numeric`, the centroid size.

## Details

This function can be used to integrate size - if meaningful - to Coo
objects. See also
[coo_length](http://momx.github.io/Momocs/reference/coo_length.md) and
[rescale](http://momx.github.io/Momocs/reference/rescale.md).

## See also

Other centroid functions:
[`coo_centdist()`](http://momx.github.io/Momocs/reference/coo_centdist.md),
[`coo_centpos()`](http://momx.github.io/Momocs/reference/coo_centpos.md)

## Examples

``` r
coo_centsize(bot[1])
#> [1] 364.1006
# on a Coo
coo_centsize(bot)
#>         brahma          caney         chimay         corona    deusventrue 
#>       364.1006       332.6606       232.2377       267.1846       300.2182 
#>          duvel   franziskaner     grimbergen        guiness     hoegardeen 
#>       220.3785       289.6220       268.2272       256.6651       353.2312 
#>        jupiler     kingfisher       latrappe lindemanskriek    nicechouffe 
#>       325.0180       238.2959       275.5208       275.0680       230.9909 
#>     pecheresse   sierranevada     tanglefoot          tauro      westmalle 
#>       310.0406       230.7661       248.5782       325.6573       255.6335 
#>          amrut    ballantines      bushmills         chivas        dalmore 
#>       287.7783       259.1542       297.9153       283.8156       247.1982 
#>   famousgrouse    glendronach   glenmorangie   highlandpark    jackdaniels 
#>       204.7011       274.2665       328.5136       252.6141       274.6856 
#>             jb  johnniewalker       magallan     makersmark           oban 
#>       340.9851       114.5988       244.4261       297.1638       283.9853 
#>     oldpotrero      redbreast         tamdhu     wildturkey         yoichi 
#>       208.3185       150.1516       337.8220       374.5002       249.7048 
# add it to $fac
mutate(bot, size=coo_centsize(bot))
#> Out (outlines)
#>   - 40 outlines, 162 +/- 21 coords (in $coo)
#>   - 3 classifiers (in $fac): 
#> # A tibble: 40 × 3
#>   type   fake   size
#>   <fct>  <fct> <dbl>
#> 1 whisky a      364.
#> 2 whisky a      333.
#> 3 whisky a      232.
#> 4 whisky a      267.
#> 5 whisky a      300.
#> 6 whisky a      220.
#> # ℹ 34 more rows
#>   - also: $ldk
```
