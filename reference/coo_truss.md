# Truss measurement

A method to calculate on shapes or on
[Coo](http://momx.github.io/Momocs/reference/Coo.md) truss measurements,
that is all pairwise combinations of euclidean distances

## Usage

``` r
coo_truss(x)
```

## Arguments

- x:

  a shape or an Ldk object

## Value

a named numeric or matrix

## Note

Mainly implemented for historical/didactical reasons.

## See also

Other premodern:
[`measure()`](http://momx.github.io/Momocs/reference/measure.md)

## Examples

``` r
# example on a single shape
cat <- coo_sample(shapes[4], 6)
coo_truss(cat)
#>       1-2       1-3       1-4       1-5       1-6       2-3       2-4       2-5 
#>  58.79626  73.24616  92.45539 165.89454  63.97656  14.76482 111.87940 214.40616 
#>       2-6       3-4       3-5       3-6       4-5       4-6       5-6 
#> 120.20815 118.00424 225.44179 133.68620 123.90722  86.14523 106.01887 

# example on wings dataset
tx <- coo_truss(wings)

txp <- PCA(tx, scale. = TRUE, center=TRUE, fac=wings$fac)
plot(txp, 1)
#> will be deprecated soon, see ?plot_PCA
```
