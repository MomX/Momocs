# Calculates the calliper length

Also called the Feret's diameter, the longest distance between two
points of the shape provided.

## Usage

``` r
coo_calliper(coo, arr.ind = FALSE)
```

## Arguments

- coo:

  a `matrix` of (x; y) coordinates or any `Coo`

- arr.ind:

  `logical`, see below.

## Value

`numeric`, the centroid size. If `arr.ind=TRUE`, a `data_frame`.

## See also

Other coo\_ utilities:
[`coo_align()`](http://momx.github.io/Momocs/reference/coo_align.md),
[`coo_aligncalliper()`](http://momx.github.io/Momocs/reference/coo_aligncalliper.md),
[`coo_alignminradius()`](http://momx.github.io/Momocs/reference/coo_alignminradius.md),
[`coo_alignxax()`](http://momx.github.io/Momocs/reference/coo_alignxax.md),
[`coo_baseline()`](http://momx.github.io/Momocs/reference/coo_baseline.md),
[`coo_bookstein()`](http://momx.github.io/Momocs/reference/coo_bookstein.md),
[`coo_boundingbox()`](http://momx.github.io/Momocs/reference/coo_boundingbox.md),
[`coo_centdist()`](http://momx.github.io/Momocs/reference/coo_centdist.md),
[`coo_center()`](http://momx.github.io/Momocs/reference/coo_center.md),
[`coo_centpos()`](http://momx.github.io/Momocs/reference/coo_centpos.md),
[`coo_close()`](http://momx.github.io/Momocs/reference/coo_close.md),
[`coo_down()`](http://momx.github.io/Momocs/reference/coo_down.md),
[`coo_dxy()`](http://momx.github.io/Momocs/reference/coo_dxy.md),
[`coo_extract()`](http://momx.github.io/Momocs/reference/coo_extract.md),
[`coo_flipx()`](http://momx.github.io/Momocs/reference/coo_flip.md),
[`coo_force2close()`](http://momx.github.io/Momocs/reference/coo_force2close.md),
[`coo_interpolate()`](http://momx.github.io/Momocs/reference/coo_interpolate.md),
[`coo_is_closed()`](http://momx.github.io/Momocs/reference/coo_is_closed.md),
[`coo_jitter()`](http://momx.github.io/Momocs/reference/coo_jitter.md),
[`coo_left()`](http://momx.github.io/Momocs/reference/coo_left.md),
[`coo_likely_clockwise()`](http://momx.github.io/Momocs/reference/coo_likely_clockwise.md),
[`coo_nb()`](http://momx.github.io/Momocs/reference/coo_nb.md),
[`coo_perim()`](http://momx.github.io/Momocs/reference/coo_perim.md),
[`coo_range()`](http://momx.github.io/Momocs/reference/coo_range.md),
[`coo_rev()`](http://momx.github.io/Momocs/reference/coo_rev.md),
[`coo_right()`](http://momx.github.io/Momocs/reference/coo_right.md),
[`coo_rotate()`](http://momx.github.io/Momocs/reference/coo_rotate.md),
[`coo_rotatecenter()`](http://momx.github.io/Momocs/reference/coo_rotatecenter.md),
[`coo_sample()`](http://momx.github.io/Momocs/reference/coo_sample.md),
[`coo_sample_prop()`](http://momx.github.io/Momocs/reference/coo_sample_prop.md),
[`coo_samplerr()`](http://momx.github.io/Momocs/reference/coo_samplerr.md),
[`coo_scale()`](http://momx.github.io/Momocs/reference/coo_scale.md),
[`coo_shearx()`](http://momx.github.io/Momocs/reference/coo_shear.md),
[`coo_slice()`](http://momx.github.io/Momocs/reference/coo_slice.md),
[`coo_slide()`](http://momx.github.io/Momocs/reference/coo_slide.md),
[`coo_slidedirection()`](http://momx.github.io/Momocs/reference/coo_slidedirection.md),
[`coo_slidegap()`](http://momx.github.io/Momocs/reference/coo_slidegap.md),
[`coo_smooth()`](http://momx.github.io/Momocs/reference/coo_smooth.md),
[`coo_smoothcurve()`](http://momx.github.io/Momocs/reference/coo_smoothcurve.md),
[`coo_template()`](http://momx.github.io/Momocs/reference/coo_template.md),
[`coo_trans()`](http://momx.github.io/Momocs/reference/coo_trans.md),
[`coo_trim()`](http://momx.github.io/Momocs/reference/coo_trim.md),
[`coo_trimbottom()`](http://momx.github.io/Momocs/reference/coo_trimbottom.md),
[`coo_trimtop()`](http://momx.github.io/Momocs/reference/coo_trimtop.md),
[`coo_untiltx()`](http://momx.github.io/Momocs/reference/coo_untiltx.md),
[`coo_up()`](http://momx.github.io/Momocs/reference/coo_up.md),
[`is_equallyspacedradii()`](http://momx.github.io/Momocs/reference/is_equallyspacedradii.md)

## Examples

``` r
b <- bot[1]
coo_calliper(b)
#> [1] 1088.166
p <- coo_calliper(b, arr.ind=TRUE)
p
#> # A tibble: 1 × 2
#>   length arr_ind  
#>    <dbl> <list>   
#> 1  1088. <dbl [2]>
p$length
#> [1] 1088.166
ids <- p$arr_ind[[1]]
coo_plot(b)
segments(b[ids[1], 1], b[ids[1], 2], b[ids[2], 1], b[ids[2], 2], lty=2)


# on a Coo
bot %>%
coo_sample(32) %>% # for speed sake
coo_calliper()
#> $brahma
#> [1] 1087.768
#> 
#> $caney
#> [1] 992.2107
#> 
#> $chimay
#> [1] 644.5991
#> 
#> $corona
#> [1] 806.6778
#> 
#> $deusventrue
#> [1] 880.8053
#> 
#> $duvel
#> [1] 606.7462
#> 
#> $franziskaner
#> [1] 863.4501
#> 
#> $grimbergen
#> [1] 766.5801
#> 
#> $guiness
#> [1] 743.6162
#> 
#> $hoegardeen
#> [1] 1046.608
#> 
#> $jupiler
#> [1] 981.2747
#> 
#> $kingfisher
#> [1] 717.4761
#> 
#> $latrappe
#> [1] 746.2345
#> 
#> $lindemanskriek
#> [1] 819.0562
#> 
#> $nicechouffe
#> [1] 686.7001
#> 
#> $pecheresse
#> [1] 927.4034
#> 
#> $sierranevada
#> [1] 655.6706
#> 
#> $tanglefoot
#> [1] 690.334
#> 
#> $tauro
#> [1] 983.9842
#> 
#> $westmalle
#> [1] 765.7114
#> 
#> $amrut
#> [1] 864.1209
#> 
#> $ballantines
#> [1] 711.5118
#> 
#> $bushmills
#> [1] 882.1485
#> 
#> $chivas
#> [1] 794.3198
#> 
#> $dalmore
#> [1] 683.668
#> 
#> $famousgrouse
#> [1] 607.8199
#> 
#> $glendronach
#> [1] 821.1796
#> 
#> $glenmorangie
#> [1] 986.0183
#> 
#> $highlandpark
#> [1] 705.139
#> 
#> $jackdaniels
#> [1] 798.2042
#> 
#> $jb
#> [1] 1011.163
#> 
#> $johnniewalker
#> [1] 337.8772
#> 
#> $magallan
#> [1] 756.595
#> 
#> $makersmark
#> [1] 858.3298
#> 
#> $oban
#> [1] 858.7974
#> 
#> $oldpotrero
#> [1] 596.5668
#> 
#> $redbreast
#> [1] 425.3011
#> 
#> $tamdhu
#> [1] 1007.425
#> 
#> $wildturkey
#> [1] 1099.426
#> 
#> $yoichi
#> [1] 714.077
#> 

bot %>%
coo_sample(32) %>% # for speed sake
coo_calliper(arr.ind=TRUE)
#> # A tibble: 40 × 2
#>    length arr_ind  
#>  *  <dbl> <list>   
#>  1  1088. <dbl [2]>
#>  2   992. <dbl [2]>
#>  3   645. <dbl [2]>
#>  4   807. <dbl [2]>
#>  5   881. <dbl [2]>
#>  6   607. <dbl [2]>
#>  7   863. <dbl [2]>
#>  8   767. <dbl [2]>
#>  9   744. <dbl [2]>
#> 10  1047. <dbl [2]>
#> # ℹ 30 more rows
```
