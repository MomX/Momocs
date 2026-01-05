# Calculate abscissa and ordinate on a shape

A simple wrapper to calculate dxi - dx1 and dyi - dx1.

## Usage

``` r
coo_dxy(coo)
```

## Arguments

- coo:

  a matrix (or a list) of (x; y) coordinates or any `Coo`

## Value

a `data.frame` with two components `dx` and `dy` for single shapes or a
`list` of such `data.frame`s for `Coo`

## See also

Other coo\_ utilities:
[`coo_align()`](http://momx.github.io/Momocs/reference/coo_align.md),
[`coo_aligncalliper()`](http://momx.github.io/Momocs/reference/coo_aligncalliper.md),
[`coo_alignminradius()`](http://momx.github.io/Momocs/reference/coo_alignminradius.md),
[`coo_alignxax()`](http://momx.github.io/Momocs/reference/coo_alignxax.md),
[`coo_baseline()`](http://momx.github.io/Momocs/reference/coo_baseline.md),
[`coo_bookstein()`](http://momx.github.io/Momocs/reference/coo_bookstein.md),
[`coo_boundingbox()`](http://momx.github.io/Momocs/reference/coo_boundingbox.md),
[`coo_calliper()`](http://momx.github.io/Momocs/reference/coo_calliper.md),
[`coo_centdist()`](http://momx.github.io/Momocs/reference/coo_centdist.md),
[`coo_center()`](http://momx.github.io/Momocs/reference/coo_center.md),
[`coo_centpos()`](http://momx.github.io/Momocs/reference/coo_centpos.md),
[`coo_close()`](http://momx.github.io/Momocs/reference/coo_close.md),
[`coo_down()`](http://momx.github.io/Momocs/reference/coo_down.md),
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
coo_dxy(coo_sample(bot[1], 12))
#> # A tibble: 12 × 2
#>       dx    dy
#>    <dbl> <dbl>
#>  1     0     0
#>  2    26  -200
#>  3     5  -411
#>  4   106  -546
#>  5   279  -448
#>  6   268  -259
#>  7   258   -38
#>  8   259   152
#>  9   203   351
#> 10   168   540
#> 11    73   441
#> 12    45   220

bot %>%
    slice(1:5) %>% coo_sample(12) %>%  # for readability and speed only
    coo_dxy()
#> $brahma
#> # A tibble: 12 × 2
#>       dx    dy
#>    <dbl> <dbl>
#>  1     0     0
#>  2    26  -200
#>  3     5  -411
#>  4   106  -546
#>  5   279  -448
#>  6   268  -259
#>  7   258   -38
#>  8   259   152
#>  9   203   351
#> 10   168   540
#> 11    73   441
#> 12    45   220
#> 
#> $caney
#> # A tibble: 12 × 2
#>       dx    dy
#>    <dbl> <dbl>
#>  1     0     0
#>  2     0  -192
#>  3     0  -373
#>  4    91  -507
#>  5   251  -436
#>  6   258  -244
#>  7   258   -73
#>  8   219   109
#>  9   193   299
#> 10   182   474
#> 11    76   392
#> 12    58   211
#> 
#> $chimay
#> # A tibble: 12 × 2
#>       dx    dy
#>    <dbl> <dbl>
#>  1     0     0
#>  2     3  -131
#>  3     7  -254
#>  4    99  -320
#>  5   227  -304
#>  6   242  -185
#>  7   239   -54
#>  8   209    83
#>  9   163   204
#> 10   145   321
#> 11    65   265
#> 12    49   145
#> 
#> $corona
#> # A tibble: 12 × 2
#>       dx    dy
#>    <dbl> <dbl>
#>  1     0     0
#>  2     0  -155
#>  3     3  -298
#>  4    73  -409
#>  5   184  -346
#>  6   185  -201
#>  7   185   -46
#>  8   155   106
#>  9   145   246
#> 10   125   396
#> 11    53   309
#> 12    33   171
#> 
#> $deusventrue
#> # A tibble: 12 × 2
#>       dx    dy
#>    <dbl> <dbl>
#>  1     0     0
#>  2   -38  -171
#>  3   -28  -334
#>  4    86  -427
#>  5   231  -384
#>  6   245  -209
#>  7   234   -47
#>  8   168   123
#>  9   151   294
#> 10   137   451
#> 11    56   347
#> 12    57   197
#> 
```
