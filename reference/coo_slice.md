# Slices shapes between successive coordinates

Takes a shape with n coordinates. When you pass this function with at
least two ids (\<= n), the shape will be open on the corresponding
coordinates and slices returned as a list

## Usage

``` r
coo_slice(coo, ids, ldk)
```

## Arguments

- coo:

  `matrix` of `(x; y)` coordinates or any
  [Coo](http://momx.github.io/Momocs/reference/Coo.md) object.

- ids:

  `numeric` of length \>= 2, where to slice the shape(s)

- ldk:

  `numeric` the id of the ldk to use as ids, only on `Out` and `Opn`. If
  provided, `ids` will be ignored.

## Value

a list of shapes or a list of
[Opn](http://momx.github.io/Momocs/reference/Opn.md)

## See also

Have a look to
[coo_slidegap](http://momx.github.io/Momocs/reference/coo_slidegap.md)
if you have problems with gaps after slicing around landmarks and/or
starting points.

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
h <- slice(hearts, 1:5)  # speed purpose only
# single shape, a list of matrices is returned
sh <- coo_slice(h[1], c(12, 24, 36, 48))
coo_plot(sh[[1]])

panel(Opn(sh))

# on a Coo, a list of Opn is returned
# makes no sense if shapes are not normalized first
sh2 <- coo_slice(h, c(12, 24, 36, 48))
panel(sh2[[1]])


# Use coo_slice with `ldk` instead:
# hearts as an example
x <- h %>% fgProcrustes(tol=1)
#> iteration:  1    gain: 8.1326 
#> iteration:  2    gain: 0.00031224 
# 4 landmarks
stack(x)

x$ldk[1:5]
#> [[1]]
#> [1] 65 56 50 19
#> 
#> [[2]]
#> [1] 69 60 52 21
#> 
#> [[3]]
#> [1] 68 60 51 21
#> 
#> [[4]]
#> [1] 69 59 53 23
#> 
#> [[5]]
#> [1] 71 61 54 21
#> 

# here we slice
y <- coo_slice(x, ldk=1:4)

# plotting
stack(y[[1]])

stack(y[[2]])


# new ldks from tipping points, new ldks from angle
olea %>% slice(1:5) %>% # for the sake of speed
def_ldk_tips %>%
def_ldk_angle(0.75*pi) %>% def_ldk_angle(0.25*pi) %>%
coo_slice(ldk =1:4) -> oleas
oleas[[1]] %>% stack

oleas[[2]] %>% stack # etc.


# domestic operations
y[[3]] %>% coo_area()
#>        shp1        shp2        shp3        shp4        shp5 
#> 0.001684956 0.007028829 0.010968094 0.009962128 0.016920135 
# shape analysis of a slice
y[[1]] %>% coo_bookstein() %>% npoly %>% PCA %>% plot(~aut)
#> 'nb.pts' missing and set to: 31
#> 'degree' missing and set to: 5
#> will be deprecated soon, see ?plot_PCA

```
