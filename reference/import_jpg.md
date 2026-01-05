# Extract outline coordinates from multiple .jpg files

This function is used to import outline coordinates and is built around
[import_jpg1](http://momx.github.io/Momocs/reference/import_jpg1.md).

## Usage

``` r
import_jpg(
  jpg.paths = .lf.auto(),
  auto.notcentered = TRUE,
  fun.notcentered = NULL,
  threshold = 0.5
)
```

## Arguments

- jpg.paths:

  a vector of paths corresponding to the .jpg files to import. If not
  provided (or `NULL`), switches to the automatic version. See Details
  below.

- auto.notcentered:

  logical if TRUE random locations will be used until. one of them is
  (assumed) to be within the shape (because of a black pixel); if FALSE
  a [locator](https://rdrr.io/r/graphics/locator.html) will be called,
  and you will have to click on a point within the shape.

- fun.notcentered:

  NULL by default. Is your shapes are not centered and if a random pick
  of a black pixel is not satisfactory. See
  [import_jpg1](http://momx.github.io/Momocs/reference/import_jpg1.md)
  help and examples.

- threshold:

  the threshold value use to binarize the images. Above, pixels are
  turned to 1, below to 0.

## Value

a list of matrices of (x; y) coordinates that can be passed to
[Out](http://momx.github.io/Momocs/reference/Out.md)

## Details

see [import_jpg1](http://momx.github.io/Momocs/reference/import_jpg1.md)
for important informations about how the outlines are extracted, and
[import_Conte](http://momx.github.io/Momocs/reference/import_Conte.md)
for the algorithm itself.

If `jpg.paths` is not provided (or `NULL`), you will have to select any
`.jpg` file in the folder that contains all your files. All the outlines
should be imported then.

## Note

Note this function will be deprecated from Momocs when `Momacs` and
`Momit` will be fully operationnal.

Silent message and progress bars (if any) with
`options("verbose"=FALSE)`.

## See also

Other import functions:
[`import_Conte()`](http://momx.github.io/Momocs/reference/import_Conte.md),
[`import_StereoMorph_curve1()`](http://momx.github.io/Momocs/reference/import_StereoMorph.md),
[`import_jpg1()`](http://momx.github.io/Momocs/reference/import_jpg1.md),
[`import_tps()`](http://momx.github.io/Momocs/reference/import_tps.md),
[`import_txt()`](http://momx.github.io/Momocs/reference/import_txt.md),
[`pix2chc()`](http://momx.github.io/Momocs/reference/babel.md)

## Examples

``` r
# \donttest{

lf <- list.files('/foo/jpegs', full.names=TRUE)
coo <- import_jpg(lf)
#> Extracting 0.jpg outlines...
#> Done in 0 secs
Out(coo)
#> empty Out

coo <- import_jpg()
#> Warning: unable to translate 'p^x<87>LV' to a wide string
#> Warning: input string 1 is invalid
#> Extracting 0.jpg outlines...
#> Done in 0 secs
# }
```
