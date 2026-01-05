# Import a tps file

And returns a list of coordinates, curves, scale

## Usage

``` r
import_tps(tps.path, curves = TRUE)

tps2coo(tps, curves = TRUE)
```

## Arguments

- tps.path:

  lines, typically from
  [readLines](https://rdrr.io/r/base/readLines.html), describing a
  single shape in tps-like format. You will need to manually build your
  `Coo` object from it: eg `Out(coo=your_list$coo)`.

- curves:

  `logical` whether to read curves, if any

- tps:

  lines for a single tps file `tps2coo` is used in import_tps and may be
  useful for data import. When provided with lines (eg after
  [readLines](https://rdrr.io/r/base/readLines.html)) from a tps-like
  description (with "LM", "CURVES", etc.) returns a list of coordinates,
  curves, etc.

## Value

a list with components: `coo` a matrix of coordinates; `cur` a list of
matrices; `scale` the scale as a numeric.

## Note

Note this function will be deprecated from Momocs when `Momacs` and
`Momit` will be fully operationnal.

## See also

Other import functions:
[`import_Conte()`](http://momx.github.io/Momocs/reference/import_Conte.md),
[`import_StereoMorph_curve1()`](http://momx.github.io/Momocs/reference/import_StereoMorph.md),
[`import_jpg()`](http://momx.github.io/Momocs/reference/import_jpg.md),
[`import_jpg1()`](http://momx.github.io/Momocs/reference/import_jpg1.md),
[`import_txt()`](http://momx.github.io/Momocs/reference/import_txt.md),
[`pix2chc()`](http://momx.github.io/Momocs/reference/babel.md)

Other import functions:
[`import_Conte()`](http://momx.github.io/Momocs/reference/import_Conte.md),
[`import_StereoMorph_curve1()`](http://momx.github.io/Momocs/reference/import_StereoMorph.md),
[`import_jpg()`](http://momx.github.io/Momocs/reference/import_jpg.md),
[`import_jpg1()`](http://momx.github.io/Momocs/reference/import_jpg1.md),
[`import_txt()`](http://momx.github.io/Momocs/reference/import_txt.md),
[`pix2chc()`](http://momx.github.io/Momocs/reference/babel.md)
