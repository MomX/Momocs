# Exports Coe objects and shapes

Writes a `.txt` or `.xls` or whatever readable from a single shape, a
[Coe](http://momx.github.io/Momocs/reference/Coe.md), or a
[PCA](http://momx.github.io/Momocs/reference/PCA.md) object, along with
individual names and `$fac`.

## Usage

``` r
export(x, file, sep, dec)
```

## Arguments

- x:

  a `Coe` or `PCA` object

- file:

  the filenames `data.txt` by default

- sep:

  the field separator string to feed
  [write.table](https://rdrr.io/r/utils/write.table.html)). (default to
  tab) tab by default

- dec:

  the string to feed
  [write.table](https://rdrr.io/r/utils/write.table.html)) (default
  `"."`) by default.

## Value

an external file

## Note

This is a simple wrapper around
[write.table](https://rdrr.io/r/utils/write.table.html).

Default parameters will write a `.txt` file, readable by foreign
programs. With default parameters, numbers will use dots as decimal
points, which is considered as a character chain in Excel in many
countries (locale versions). This can be solved by using `dec=','` as in
the examples below.

If you are looking for your file, and did not specified `file`,
[`getwd()`](https://rdrr.io/r/base/getwd.html) will help.

I have to mention that everytime you use this function, and cowardly run
from R to Excel and do 'statistics' there, an innocent and adorable
kitten is probably murdered somewhere. Use R!

## See also

Other bridges functions:
[`as_df()`](http://momx.github.io/Momocs/reference/as_df.md),
[`bridges`](http://momx.github.io/Momocs/reference/bridges.md),
[`complex`](http://momx.github.io/Momocs/reference/complex.md)

## Examples

``` r
# Will write (and remove) files on your working directory!
if (FALSE) { # \dontrun{
bf <- efourier(bot, 6)
# Export Coe (here Fourier coefficients)
export(bf) # data.txt which can be opened by every software including MS Excel

# If you come from a country that uses comma as decimal separator (not recommended, but...)
export(bf, dec=',')
export(bf, file='data.xls', dec=',')

# Export PCA scores
bf %>% PCA %>% export()

# for shapes (matrices)
# export(bot[1], file='bot1.txt')

 # remove these files from your machine
 file.remove("coefficients.txt", "data.xls", "scores.txt")
} # }
```
