# Rescale coordinates from pixels to real length units

Most of the time, (x, y) coordinates are recorded in pixels. If we want
to have them in mm, cm, etc. we need to convert them and to rescale
them. This functions does the job for the two cases: i) either an
homogeneous rescaling factor, e.g. if all pictures were taken using the
very same magnification or ii) with various magnifications. More in the
Details section

## Usage

``` r
rescale(x, scaling_factor, scale_mapping, magnification_col, ...)
```

## Arguments

- x:

  any `Coo` object

- scaling_factor:

  numeric an homogeneous scaling factor. If all you (x, y) coordinates
  have the same scale

- scale_mapping:

  either a data.frame or a path to read such a data.frame. It MUST
  contain three columns in that order: magnification found in `$fac`,
  column `"magnification_col"`, pixels, real length unit. Column names
  do not matter but must be specified, as read.table reads with
  `header=TRUE` Every different magnification level found in `$fac`,
  column `"magnification_col"` must have its row.

- magnification_col:

  the name or id of the \$fac column to look for magnification levels
  for every image

- ...:

  additional arguments (besides header=TRUE) to pass to read.table if
  'scale_mapping' is a path

## Value

a Momocs object of same class

## Details

The i) case above is straightforward, if 1cm is 500pix long on all your
pictures, just call `rescale(your_Coo, scaling_factor=1/500)` and all
coordinates will be in cm.

The ii) second case is more subtle. First you need to code in your
[Coo](http://momx.github.io/Momocs/reference/Coo.md) object, in the fac
slot, a column named, say "mag", for magnification. Imagine you have 4
magnifications: 0.5, 1, 2 and 5, we have to indicate for each
magnification, how many pixels stands for how many units in the real
world.

This information is passed as a data.frame, built externally or in R,
that must look like this:

    mag   pix    cm
    0.5   1304   10
    1     921    10
    2     816    5
    5     1020   5

.

We have to do that because, for optical reasons, the ratio
pix/real_unit, is not a linear function of the magnification.

All shapes will be centered to apply (the single or the different)
scaling_factor.

## Note

This function is simple but quite complex to detail. Feel free to
contact me should you have any problem with it. You can just access its
code (type `rescale`) and reply it yourself.

## See also

Other handling functions:
[`arrange()`](http://momx.github.io/Momocs/reference/arrange.md),
[`at_least()`](http://momx.github.io/Momocs/reference/at_least.md),
[`chop()`](http://momx.github.io/Momocs/reference/chop.md),
[`combine()`](http://momx.github.io/Momocs/reference/combine.md),
[`dissolve()`](http://momx.github.io/Momocs/reference/dissolve.md),
[`fac_dispatcher()`](http://momx.github.io/Momocs/reference/fac_dispatcher.md),
[`filter()`](http://momx.github.io/Momocs/reference/filter.md),
[`mutate()`](http://momx.github.io/Momocs/reference/mutate.md),
[`rename()`](http://momx.github.io/Momocs/reference/rename.md),
[`rm_harm()`](http://momx.github.io/Momocs/reference/rm_harm.md),
[`rm_missing()`](http://momx.github.io/Momocs/reference/rm_missing.md),
[`rm_uncomplete()`](http://momx.github.io/Momocs/reference/rm_uncomplete.md),
[`rw_fac()`](http://momx.github.io/Momocs/reference/rw_fac.md),
[`sample_frac()`](http://momx.github.io/Momocs/reference/sample_frac.md),
[`sample_n()`](http://momx.github.io/Momocs/reference/sample_n.md),
[`select()`](http://momx.github.io/Momocs/reference/select.md),
[`slice()`](http://momx.github.io/Momocs/reference/slice.md),
[`subsetize()`](http://momx.github.io/Momocs/reference/subset.md)
