# Plots a .jpg image

A very simple image plotter. If provided with a path, reads the .jpg and
plots it. If not provided with an imagematrix, will ask you to choose
interactively a `.jpeg` image.

## Usage

``` r
img_plot(img)

img_plot0(img)
```

## Arguments

- img:

  a matrix of an image, such as those obtained with readJPEG.

## Value

a plot

## Details

`img_plot` is used in import functions such as
[import_jpg1](http://momx.github.io/Momocs/reference/import_jpg1.md);
`img_plot0` does the same job but preserves the `par` and plots axes.
