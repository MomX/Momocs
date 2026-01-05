# Calculate centroid coordinates

Returns the (x; y) centroid coordinates of a shape.

## Usage

``` r
coo_centpos(coo)
```

## Arguments

- coo:

  `matrix` of `(x; y)` coordinates or any
  [Coo](http://momx.github.io/Momocs/reference/Coo.md) object.

## Value

(x; y) coordinates of the centroid as a vector or a matrix.

## See also

Other centroid functions:
[`coo_centdist()`](http://momx.github.io/Momocs/reference/coo_centdist.md),
[`coo_centsize()`](http://momx.github.io/Momocs/reference/coo_centsize.md)

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
coo_plot(b)
xy <- coo_centpos(b)
points(xy[1], xy[2], cex=2, col='blue')

# on a Coo
coo_centpos(bot)
#>                       x        y
#> brahma         175.0580 543.8696
#> caney          182.7083 507.7560
#> chimay         169.0106 314.8095
#> corona         185.0155 407.2326
#> deusventrue    179.5592 467.2632
#> duvel          179.2484 287.1180
#> franziskaner   161.3548 423.2016
#> grimbergen     166.7460 394.8651
#> guiness        182.2022 372.0546
#> hoegardeen     173.2539 526.9275
#> jupiler        175.6026 510.9744
#> kingfisher     161.8407 365.2253
#> latrappe       176.0368 344.0147
#> lindemanskriek 163.9261 405.4034
#> nicechouffe    170.5548 338.1233
#> pecheresse     175.3023 489.5271
#> sierranevada   166.5795 333.5682
#> tanglefoot     174.5862 346.1724
#> tauro          175.5230 511.7644
#> westmalle      161.7943 383.0000
#> amrut          162.7225 420.5654
#> ballantines    174.2260 329.5000
#> bushmills      180.8303 432.3697
#> chivas         182.0244 405.7500
#> dalmore        176.4258 328.0452
#> famousgrouse   174.1065 299.2071
#> glendronach    173.2792 409.4365
#> glenmorangie   177.2514 493.9385
#> highlandpark   167.4852 346.6272
#> jackdaniels    182.8867 387.7600
#> jb             172.6149 509.0057
#> johnniewalker  174.4940 165.5655
#> magallan       167.2482 388.9149
#> makersmark     176.4802 402.7571
#> oban           176.5307 447.6536
#> oldpotrero     165.9160 284.7634
#> redbreast      176.8305 202.1977
#> tamdhu         173.7955 530.5625
#> wildturkey     173.7243 537.4973
#> yoichi         181.2764 361.1545
```
