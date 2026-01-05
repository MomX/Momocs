# Harmonic contribution to shape

Calculates contribution of harmonics to shape. The amplitude of every
coefficients of a given harmonic is multiplied by the coefficients
provided and the resulting shapes are reconstructed and plotted.
Naturally, only works on Fourier-based methods.

## Usage

``` r
hcontrib(Coe, ...)

# S3 method for class 'OutCoe'
hcontrib(
  Coe,
  id,
  harm.r,
  amp.r = c(0, 0.5, 1, 2, 5, 10),
  main = "Harmonic contribution to shape",
  xlab = "Harmonic rank",
  ylab = "Amplification factor",
  ...
)
```

## Arguments

- Coe:

  a [`Coe`](http://momx.github.io/Momocs/reference/Coe.md) object
  (either `OutCoe` or (soon) `OpnCoe`)

- ...:

  additional parameter to pass to
  [`coo_draw`](http://momx.github.io/Momocs/reference/coo_draw.md)

- id:

  the id of a particular shape, otherwise working on the meanshape

- harm.r:

  range of harmonics on which to explore contributions

- amp.r:

  a vector of numeric for multiplying coefficients

- main:

  a title for the plot

- xlab:

  a title for the x-axis

- ylab:

  a title for the y-axis

## Value

a plot

## See also

Other Coe_graphics:
[`boxplot.OutCoe()`](http://momx.github.io/Momocs/reference/boxplot.OutCoe.md)

## Examples

``` r
data(bot)
bot.f <- efourier(bot, 12)
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details
hcontrib(bot.f)
#> no 'id' provided, working on the meanshape

hcontrib(bot.f, harm.r=3:10, amp.r=1:8, col="grey20",
   main="A huge panel")
#> no 'id' provided, working on the meanshape
```
