# "Redo" a PCA on a new Coe

Basically reapply rotation to a new Coe object.

## Usage

``` r
rePCA(PCA, Coe)
```

## Arguments

- PCA:

  a [PCA](http://momx.github.io/Momocs/reference/PCA.md) object

- Coe:

  a [Coe](http://momx.github.io/Momocs/reference/Coe.md) object

## Note

Quite experimental. Dimensions of the matrices and methods must match.

## Examples

``` r
b <- filter(bot, type=="beer")
w <- filter(bot, type=="whisky")

bf <- efourier(b, 8)
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details
bp <- PCA(bf)

wf <- efourier(w, 8)
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details

# and we use the "beer" PCA on the whisky coefficients
wp <- rePCA(bp, wf)

plot(wp)
#> will be deprecated soon, see ?plot_PCA


plot(bp, eig=FALSE)
#> will be deprecated soon, see ?plot_PCA
points(wp$x[, 1:2], col="red", pch=4)

```
