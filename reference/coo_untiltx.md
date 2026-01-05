# Removes rotation so that the centroid and a given point are parallel to the x-axis

Rotationnal biases appear after
[coo_slidedirection](http://momx.github.io/Momocs/reference/coo_slidedirection.md)
(and friends). Typically useful for outline analysis where phasing
matters. See examples.

## Usage

``` r
coo_untiltx(coo, id, ldk)
```

## Arguments

- coo:

  `matrix` of `(x; y)` coordinates or any
  [Coo](http://momx.github.io/Momocs/reference/Coo.md) object.

- id:

  `numeric` the id of the point that will become the new first point.
  See details below for the method on Coo objects.

- ldk:

  `numeric` the id of the ldk to use as id, only on `Out`

## Value

a `matrix` of (x; y) coordinates, or a
[Coo](http://momx.github.io/Momocs/reference/Coo.md) object.

## Details

For Coo objects, and in particular for Out and Opn two different ways of
coo_sliding are available:

- **no ldk passed and an id is passed**: all id-th points within the
  shapes will become the first points.

- **a single ldk is passed**: the ldk-th ldk will be used to slide every
  shape. If an id is (also) passed, id is ignored with a message.

## See also

[coo_slide](http://momx.github.io/Momocs/reference/coo_slide.md) and
friends.

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
[`coo_up()`](http://momx.github.io/Momocs/reference/coo_up.md),
[`is_equallyspacedradii()`](http://momx.github.io/Momocs/reference/is_equallyspacedradii.md)

## Examples

``` r
# on a single shape
bot[1] %>% coo_center %>% coo_align %>%
   coo_sample(12) %>% coo_slidedirection("right") %T>%
   coo_plot() %>% # the first point is not on the x-axis
   coo_untiltx() %>%
   coo_draw(border="red") # this (red) one is


# on an Out
# prepare bot
prebot <- bot %>% coo_center %>% coo_scale %>%
   coo_align %>% coo_slidedirection("right")
prebot %>% stack # some dephasing remains

prebot %>% coo_slidedirection("right") %>% coo_untiltx() %>% stack # much better

# _here_ there is no change but the second, untilted, is correct
prebot %>% efourier(8, norm=FALSE) %>% PCA %>% plot_PCA(~type)

prebot %>% coo_untiltx %>% efourier(8, norm=FALSE) %>% PCA %>% plot_PCA(~type)


# an example using ldks:
# the landmark #2 is on the x-axis
hearts %>%
  slice(1:5) %>% fgProcrustes(tol=1e-3) %>% # for speed sake
  coo_center %>% coo_untiltx(ldk=2) %>% stack
#> iteration:  1    gain: 8.1326 
#> iteration:  2    gain: 0.00031224 
```
