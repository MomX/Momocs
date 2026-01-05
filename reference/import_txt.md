# Import coordinates from a .txt file

A wrapper around [read.table](https://rdrr.io/r/utils/read.table.html)
that can be used to import outline/landmark coordinates.

## Usage

``` r
import_txt(txt.paths = .lf.auto(), ...)
```

## Arguments

- txt.paths:

  a vector of paths corresponding to the .txt files to import. If not
  provided (or `NULL`), switches to the automatic version, just as in
  [import_jpg](http://momx.github.io/Momocs/reference/import_jpg.md).
  See Details there.

- ...:

  arguments to be passed to
  [read.table](https://rdrr.io/r/utils/read.table.html), eg. 'skip',
  'dec', etc.

## Value

a list of matrix(ces) of (x; y) coordinates that can be passed to
[Out](http://momx.github.io/Momocs/reference/Out.md),
[Opn](http://momx.github.io/Momocs/reference/Opn.md) and
[Ldk](http://momx.github.io/Momocs/reference/Ldk.md).

## Details

Columns are not named in the `.txt` files. You can tune this using the
`...` argument. Define the
[read.table](https://rdrr.io/r/utils/read.table.html) arguments that
allow to import a single file, and then pass them to this function, ie
if your `.txt` file has a header (eg ('x', 'y')), do not forget
`header=TRUE`.

## Note

Note this function will be deprecated from Momocs when `Momacs` and
`Momit` will be fully operationnal.

Silent message and progress bars (if any) with
`options("verbose"=FALSE)`.

## See also

Other import functions:
[`import_Conte()`](http://momx.github.io/Momocs/reference/import_Conte.md),
[`import_StereoMorph_curve1()`](http://momx.github.io/Momocs/reference/import_StereoMorph.md),
[`import_jpg()`](http://momx.github.io/Momocs/reference/import_jpg.md),
[`import_jpg1()`](http://momx.github.io/Momocs/reference/import_jpg1.md),
[`import_tps()`](http://momx.github.io/Momocs/reference/import_tps.md),
[`pix2chc()`](http://momx.github.io/Momocs/reference/babel.md)
