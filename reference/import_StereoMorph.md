# Import files creates by StereoMorph into Momocs

Helps to read `.txt` files created by StereoMorph into (x; y)
coordinates or Momocs objects. Can be applied to 'curves' or 'ldk' text
files.

## Usage

``` r
import_StereoMorph_curve1(path)

import_StereoMorph_curve(path, names)

import_StereoMorph_ldk1(path)

import_StereoMorph_ldk(path, names)
```

## Arguments

- path:

  toward a single file or a folder containing `.txt` files produced by
  StereoMorph

- names:

  to feed
  [lf_structure](http://momx.github.io/Momocs/reference/lf_structure.md)

## Value

a list of class Coo

## Details

\*1 functions import a single `.txt` file. Their counterpart (no '1')
work when path indicates the folder, i.e. 'curves' or 'ldk'. They then
return a list of [Opn](http://momx.github.io/Momocs/reference/Opn.md) or
[Ldk](http://momx.github.io/Momocs/reference/Ldk.md) objects,
respectively. Please do not hesitate to contact me should you have a
particular case or need something.

## Note

Note this function will be deprecated from Momocs when `Momacs` and
`Momit` will be fully operationnal.

## See also

Other import functions:
[`import_Conte()`](http://momx.github.io/Momocs/reference/import_Conte.md),
[`import_jpg()`](http://momx.github.io/Momocs/reference/import_jpg.md),
[`import_jpg1()`](http://momx.github.io/Momocs/reference/import_jpg1.md),
[`import_tps()`](http://momx.github.io/Momocs/reference/import_tps.md),
[`import_txt()`](http://momx.github.io/Momocs/reference/import_txt.md),
[`pix2chc()`](http://momx.github.io/Momocs/reference/babel.md)

Other import functions:
[`import_Conte()`](http://momx.github.io/Momocs/reference/import_Conte.md),
[`import_jpg()`](http://momx.github.io/Momocs/reference/import_jpg.md),
[`import_jpg1()`](http://momx.github.io/Momocs/reference/import_jpg1.md),
[`import_tps()`](http://momx.github.io/Momocs/reference/import_tps.md),
[`import_txt()`](http://momx.github.io/Momocs/reference/import_txt.md),
[`pix2chc()`](http://momx.github.io/Momocs/reference/babel.md)

Other import functions:
[`import_Conte()`](http://momx.github.io/Momocs/reference/import_Conte.md),
[`import_jpg()`](http://momx.github.io/Momocs/reference/import_jpg.md),
[`import_jpg1()`](http://momx.github.io/Momocs/reference/import_jpg1.md),
[`import_tps()`](http://momx.github.io/Momocs/reference/import_tps.md),
[`import_txt()`](http://momx.github.io/Momocs/reference/import_txt.md),
[`pix2chc()`](http://momx.github.io/Momocs/reference/babel.md)

Other import functions:
[`import_Conte()`](http://momx.github.io/Momocs/reference/import_Conte.md),
[`import_jpg()`](http://momx.github.io/Momocs/reference/import_jpg.md),
[`import_jpg1()`](http://momx.github.io/Momocs/reference/import_jpg1.md),
[`import_tps()`](http://momx.github.io/Momocs/reference/import_tps.md),
[`import_txt()`](http://momx.github.io/Momocs/reference/import_txt.md),
[`pix2chc()`](http://momx.github.io/Momocs/reference/babel.md)
