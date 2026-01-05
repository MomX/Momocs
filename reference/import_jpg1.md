# Extract outline coordinates from a single .jpg file

Used to import outline coordinates from .jpg files. This function is
used for single images and is wrapped by
[import_jpg](http://momx.github.io/Momocs/reference/import_jpg.md). It
relies itself on
[import_Conte](http://momx.github.io/Momocs/reference/import_Conte.md)

## Usage

``` r
import_jpg1(
  jpg.path,
  auto.notcentered = TRUE,
  fun.notcentered = NULL,
  threshold = 0.5,
  ...
)
```

## Arguments

- jpg.path:

  vector of paths corresponding to the .jpg files to import, such as
  those obtained with
  [list.files](https://rdrr.io/r/base/list.files.html).

- auto.notcentered:

  logical if TRUE random locations will be used until one of them is
  (assumed) to be within the shape (because it corresponds to a black
  pixel) and only if the middle point is not black; if FALSE a
  [locator](https://rdrr.io/r/graphics/locator.html) will be called, and
  you will have to click on a point within the shape.

- fun.notcentered:

  NULL by default but can accept a function that, when passed with an
  imagematrix and returns a numeric of length two that corresponds to a
  starting point on the imagematrix for the Conte algorithm. A `while`
  instruction wraps it, so the function may be wrong in proposing this
  starting position. See the examples below for a quick example.

- threshold:

  the threshold value use to binarize the images. Above, pixels are
  turned to 1, below to 0.

- ...:

  arguments to be passed to
  [read.table](https://rdrr.io/r/utils/read.table.html), eg. 'skip',
  'dec', etc.

## Value

a matrix of (x; y) coordinates that can be passed to Out

## Details

jpegs can be provided either as RVB or as 8-bit greylevels or
monochrome. The function binarizes pixels values using the 'threshold'
argument. It will try to start to apply the
[import_Conte](http://momx.github.io/Momocs/reference/import_Conte.md)
algorithm from the center of the image and 'looking' downwards for the
first black/white 'frontier' in the pixels. This point will be the first
of the outlines. The latter may be useful if you align manually the
images and if you want to retain this information in the consequent
morphometric analyses.

If the point at the center of the image is not within the shape, i.e. is
'white' you have two choices defined by the 'auto.notcentered' argument.
If it's TRUE, some random starting points will be tried until on of them
is 'black' and within the shape; if FALSE you will be asked to click on
a point within the shape.

If some pixels on the borders are not white, this functions adds a
2-pixel border of white pixels; otherwise
[import_Conte](http://momx.github.io/Momocs/reference/import_Conte.md)
would fail and return an error.

Finally, remember that if the images are not in your working directory,
[list.files](https://rdrr.io/r/base/list.files.html) must be called with
the argument `full.names=TRUE`!

Note that the use of the `fun.notcentered` argument will probably leads
to serious headaches and will probably imply the dissection of these
functions:
[import_Conte](http://momx.github.io/Momocs/reference/import_Conte.md),
[img_plot](http://momx.github.io/Momocs/reference/img_plot.md) and
`import_jpg` itself

## Note

Note this function will be deprecated from Momocs when `Momacs` and
`Momit` will be fully operationnal.

## See also

[import_jpg](http://momx.github.io/Momocs/reference/import_jpg.md),
[import_Conte](http://momx.github.io/Momocs/reference/import_Conte.md),
[import_txt](http://momx.github.io/Momocs/reference/import_txt.md),
[lf_structure](http://momx.github.io/Momocs/reference/lf_structure.md).
See also Momocs' vignettes for data import.

Other import functions:
[`import_Conte()`](http://momx.github.io/Momocs/reference/import_Conte.md),
[`import_StereoMorph_curve1()`](http://momx.github.io/Momocs/reference/import_StereoMorph.md),
[`import_jpg()`](http://momx.github.io/Momocs/reference/import_jpg.md),
[`import_tps()`](http://momx.github.io/Momocs/reference/import_tps.md),
[`import_txt()`](http://momx.github.io/Momocs/reference/import_txt.md),
[`pix2chc()`](http://momx.github.io/Momocs/reference/babel.md)
