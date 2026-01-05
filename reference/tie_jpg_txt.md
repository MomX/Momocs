# Binds .jpg outlines from .txt landmarks taken on them

Given a list of files (lf) that includes matching filenames with .jpg
(black masks) and .txt (landmark positions on them as .txt), returns an
Out with \$ldk defined. Typically be useful if you use ImageJ to define
landmarks on your outlines.

## Usage

``` r
tie_jpg_txt(lf)
```

## Arguments

- lf:

  a list of filenames

## Value

an Out object

## Note

Not optimized (images are read twice). Please do not hesitate to contact
me should you have a particular case or need something.

## See also

Other babel functions:
[`lf_structure()`](http://momx.github.io/Momocs/reference/lf_structure.md)
