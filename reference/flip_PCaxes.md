# Flips PCA axes

Simply multiply by -1, corresponding scores and rotation vectors for PCA
objects. PC orientation being arbitrary, this may help to have a better
display.

## Usage

``` r
flip_PCaxes(x, axs)
```

## Arguments

- x:

  a PCA object

- axs:

  numeric which PC(s) to flip

## Examples

``` r
bp <- bot %>% efourier(6) %>% PCA
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details
bp %>% plot
#> will be deprecated soon, see ?plot_PCA

bp %>% flip_PCaxes(1) %>% plot()
#> will be deprecated soon, see ?plot_PCA
```
