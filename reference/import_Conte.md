# Extract outlines coordinates from an image silhouette

Provided with an image 'mask' (i.e. black pixels on a white background),
and a point form where to start the algorithm, returns the (x; y)
coordinates of its outline.

## Usage

``` r
import_Conte(img, x)
```

## Arguments

- img:

  a matrix of a binary image mask.

- x:

  numeric the (x; y) coordinates of a starting point within the shape.

## Value

a matrix the (x; y) coordinates of the outline points.

## Details

Used internally by
[import_jpg1](http://momx.github.io/Momocs/reference/import_jpg1.md) but
may be useful for other purposes.

## Note

Note this function will be deprecated from Momocs when `Momacs` and
`Momit` will be fully operationnal.

If you have an image with more than a single shape, then you may want to
try `imager::highlight` function. Momocs may use this at some point.

## References

- The original algorithm is due to: Pavlidis, T. (1982). *Algorithms for
  graphics and image processing*. Computer science press.

- is detailed in: Rohlf, F. J. (1990). An overview of image processing
  and analysis techniques for morphometrics. In *Proceedings of the
  Michigan Morphometrics Workshop*. Special Publication No. 2 (pp.
  47-60). University of Michigan Museum of Zoology: Ann Arbor.

- and translated in R by: Claude, J. (2008). *Morphometrics with R*. (p.
  316). Springer.

## See also

Other import functions:
[`import_StereoMorph_curve1()`](http://momx.github.io/Momocs/reference/import_StereoMorph.md),
[`import_jpg()`](http://momx.github.io/Momocs/reference/import_jpg.md),
[`import_jpg1()`](http://momx.github.io/Momocs/reference/import_jpg1.md),
[`import_tps()`](http://momx.github.io/Momocs/reference/import_tps.md),
[`import_txt()`](http://momx.github.io/Momocs/reference/import_txt.md),
[`pix2chc()`](http://momx.github.io/Momocs/reference/babel.md)
