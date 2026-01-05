# Test if shapes are closed

Returns TRUE/FALSE whether the last coordinate of the shapes is the same
as the first one.

## Usage

``` r
coo_is_closed(coo)

is_open(coo)
```

## Arguments

- coo:

  `matrix` of `(x; y)` coordinates or any
  [Coo](http://momx.github.io/Momocs/reference/Coo.md) object.

## Value

a single or a vector of `logical`.

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
coo_is_closed(matrix(1:10, ncol=2))
#> [1] FALSE
coo_is_closed(coo_close(matrix(1:10, ncol=2)))
#> [1] TRUE
coo_is_closed(bot)
#>         brahma          caney         chimay         corona    deusventrue 
#>          FALSE          FALSE          FALSE          FALSE          FALSE 
#>          duvel   franziskaner     grimbergen        guiness     hoegardeen 
#>          FALSE          FALSE          FALSE          FALSE          FALSE 
#>        jupiler     kingfisher       latrappe lindemanskriek    nicechouffe 
#>          FALSE          FALSE          FALSE          FALSE          FALSE 
#>     pecheresse   sierranevada     tanglefoot          tauro      westmalle 
#>          FALSE          FALSE          FALSE          FALSE          FALSE 
#>          amrut    ballantines      bushmills         chivas        dalmore 
#>          FALSE          FALSE          FALSE          FALSE          FALSE 
#>   famousgrouse    glendronach   glenmorangie   highlandpark    jackdaniels 
#>          FALSE          FALSE          FALSE          FALSE          FALSE 
#>             jb  johnniewalker       magallan     makersmark           oban 
#>          FALSE          FALSE          FALSE          FALSE          FALSE 
#>     oldpotrero      redbreast         tamdhu     wildturkey         yoichi 
#>          FALSE          FALSE          FALSE          FALSE          FALSE 
coo_is_closed(coo_close(bot))
#>         brahma          caney         chimay         corona    deusventrue 
#>           TRUE           TRUE           TRUE           TRUE           TRUE 
#>          duvel   franziskaner     grimbergen        guiness     hoegardeen 
#>           TRUE           TRUE           TRUE           TRUE           TRUE 
#>        jupiler     kingfisher       latrappe lindemanskriek    nicechouffe 
#>           TRUE           TRUE           TRUE           TRUE           TRUE 
#>     pecheresse   sierranevada     tanglefoot          tauro      westmalle 
#>           TRUE           TRUE           TRUE           TRUE           TRUE 
#>          amrut    ballantines      bushmills         chivas        dalmore 
#>           TRUE           TRUE           TRUE           TRUE           TRUE 
#>   famousgrouse    glendronach   glenmorangie   highlandpark    jackdaniels 
#>           TRUE           TRUE           TRUE           TRUE           TRUE 
#>             jb  johnniewalker       magallan     makersmark           oban 
#>           TRUE           TRUE           TRUE           TRUE           TRUE 
#>     oldpotrero      redbreast         tamdhu     wildturkey         yoichi 
#>           TRUE           TRUE           TRUE           TRUE           TRUE 
```
