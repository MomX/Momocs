# Calculates perimeter and variations

`coo_perim` calculates the perimeter; `coo_perimpts` calculates the
euclidean distance between every points of a shape; `coo_perimcum` does
the same and calculates and cumulative sum.

## Usage

``` r
coo_perimpts(coo)

# Default S3 method
coo_perimpts(coo)

# S3 method for class 'Coo'
coo_perimpts(coo)

coo_perimcum(coo)

# Default S3 method
coo_perimcum(coo)

# S3 method for class 'Coo'
coo_perimcum(coo)

coo_perim(coo)

# Default S3 method
coo_perim(coo)

# S3 method for class 'Coo'
coo_perim(coo)
```

## Arguments

- coo:

  `matrix` of (x; y) coordinates or any `Coo`

## Value

`numeric` the distance between every point or a `list` of those.

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
# for speed sake
b1 <- coo_sample(bot[1], 12)
b5 <- bot %>% slice(1:5) %>% coo_sample(12)

# coo_perim
coo_perim(b1)
#> [1] 2140.62
coo_perim(b5)
#>      brahma       caney      chimay      corona deusventrue 
#>    2140.620    1942.307    1354.083    1555.883    1768.427 

# coo_perimpts
coo_perimpts(b1)
#>  [1] 201.6829 212.0424 168.6001 198.8291 189.3198 221.2261 190.0026 206.7293
#>  [9] 192.2134 137.2079 222.7667
b5 %>% coo_perimpts()
#> $brahma
#>  [1] 201.6829 212.0424 168.6001 198.8291 189.3198 221.2261 190.0026 206.7293
#>  [9] 192.2134 137.2079 222.7667
#> 
#> $caney
#>  [1] 192.0000 181.0000 161.9784 175.0457 192.1276 171.0000 186.1317 191.7707
#>  [9] 175.3454 134.0149 181.8928
#> 
#> $chimay
#>  [1] 131.03435 123.06502 113.22544 128.99612 119.94165 131.03435 140.24621
#>  [8] 129.44883 118.37652  97.65244 121.06197
#> 
#> $corona
#>  [1] 155.0000 143.0315 131.2288 127.6323 145.0034 155.0000 154.9322 140.3567
#>  [9] 151.3275 112.9292 139.4417
#> 
#> $deusventrue
#>  [1] 175.1713 163.3065 147.1224 151.2415 175.5591 162.3730 182.3623 171.8430
#>  [9] 157.6230 131.8218 150.0033
#> 

# coo_perimcum
b1 %>% coo_perimcum()
#>  [1]    0.0000  201.6829  413.7254  582.3255  781.1546  970.4744 1191.7005
#>  [8] 1381.7032 1588.4324 1780.6459 1917.8537 2140.6204
b5 %>% coo_perimcum()
#> $brahma
#>  [1]    0.0000  201.6829  413.7254  582.3255  781.1546  970.4744 1191.7005
#>  [8] 1381.7032 1588.4324 1780.6459 1917.8537 2140.6204
#> 
#> $caney
#>  [1]    0.0000  192.0000  373.0000  534.9784  710.0241  902.1517 1073.1517
#>  [8] 1259.2833 1451.0540 1626.3994 1760.4143 1942.3072
#> 
#> $chimay
#>  [1]    0.0000  131.0343  254.0994  367.3248  496.3209  616.2626  747.2969
#>  [8]  887.5431 1016.9920 1135.3685 1233.0209 1354.0829
#> 
#> $corona
#>  [1]    0.0000  155.0000  298.0315  429.2603  556.8926  701.8960  856.8960
#>  [8] 1011.8282 1152.1849 1303.5124 1416.4416 1555.8833
#> 
#> $deusventrue
#>  [1]    0.0000  175.1713  338.4778  485.6002  636.8417  812.4008  974.7739
#>  [8] 1157.1361 1328.9791 1486.6021 1618.4239 1768.4272
#> 
```
