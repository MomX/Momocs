# Turn Momocs objects into tydy data_frames

Used in particular for compatibility with the `tidyverse`

## Usage

``` r
as_df(x, ...)

# S3 method for class 'Coo'
as_df(x, ...)

# S3 method for class 'Coe'
as_df(x, ...)

# S3 method for class 'PCA'
as_df(x, retain, ...)

# S3 method for class 'LDA'
as_df(x, retain, ...)
```

## Arguments

- x:

  an object, typically a Momocs object

- ...:

  useless here

- retain:

  numeric for use with
  [scree](http://momx.github.io/Momocs/reference/scree.md) methods.
  Defaut to all. If `<1`, enough axes to retain this proportion of
  variance; if `>1`, this number of axes.

## Value

a
[`dplyr::tibble()`](https://dplyr.tidyverse.org/reference/reexports.html)

## See also

Other bridges functions:
[`bridges`](http://momx.github.io/Momocs/reference/bridges.md),
[`complex`](http://momx.github.io/Momocs/reference/complex.md),
[`export()`](http://momx.github.io/Momocs/reference/export.md)

## Examples

``` r
# first, some (baby) objects
b <- bot %>% coo_sample(12)
bf <- b %>% efourier(5, norm=TRUE)
# Coo object
b %>% as_df
#> # A tibble: 40 × 3
#>    coo            type   fake 
#>    <named list>   <fct>  <fct>
#>  1 <dbl [12 × 2]> whisky a    
#>  2 <dbl [12 × 2]> whisky a    
#>  3 <dbl [12 × 2]> whisky a    
#>  4 <dbl [12 × 2]> whisky a    
#>  5 <dbl [12 × 2]> whisky a    
#>  6 <dbl [12 × 2]> whisky a    
#>  7 <dbl [12 × 2]> whisky a    
#>  8 <dbl [12 × 2]> whisky a    
#>  9 <dbl [12 × 2]> whisky a    
#> 10 <dbl [12 × 2]> whisky a    
#> # ℹ 30 more rows
# Coe object
bf %>% as_df
#> # A tibble: 40 × 22
#>    type   fake     A1      A2     A3      A4     A5    B1        B2       B3
#>    <fct>  <fct> <dbl>   <dbl>  <dbl>   <dbl>  <dbl> <dbl>     <dbl>    <dbl>
#>  1 whisky a         1 0.0120  0.0917 0.0124  0.0248     0 -0.00112  -0.00100
#>  2 whisky a         1 0.0110  0.0918 0.0124  0.0224     0 -0.00125  -0.00280
#>  3 whisky a         1 0.0213  0.0770 0.0240  0.0140     0 -0.00637   0.00124
#>  4 whisky a         1 0.00905 0.0960 0.00971 0.0263     0 -0.000555 -0.00204
#>  5 whisky a         1 0.0208  0.0913 0.0208  0.0193     0 -0.00108   0.00113
#>  6 whisky a         1 0.0200  0.0722 0.0213  0.0119     0 -0.00215   0.00349
#>  7 whisky a         1 0.00998 0.0912 0.0122  0.0248     0 -0.000172 -0.00124
#>  8 whisky a         1 0.0197  0.0845 0.0217  0.0164     0 -0.000464 -0.00144
#>  9 whisky a         1 0.0194  0.0864 0.0214  0.0191     0 -0.00288  -0.00196
#> 10 whisky a         1 0.0128  0.0929 0.0141  0.0236     0 -0.000998 -0.00170
#> # ℹ 30 more rows
#> # ℹ 12 more variables: B4 <dbl>, B5 <dbl>, C1 <dbl>, C2 <dbl>, C3 <dbl>,
#> #   C4 <dbl>, C5 <dbl>, D1 <dbl>, D2 <dbl>, D3 <dbl>, D4 <dbl>, D5 <dbl>

# PCA object
bf %>% PCA %>% as_df     # all PCs by default
#> `retain` is too ambitious. All axes returned
#> # A tibble: 40 × 22
#>    type   fake      PC1      PC2       PC3       PC4       PC5      PC6      PC7
#>    <fct>  <fct>   <dbl>    <dbl>     <dbl>     <dbl>     <dbl>    <dbl>    <dbl>
#>  1 whisky a     -0.0520 -0.0226   0.00517  -0.00866    1.31e-2 -1.09e-3 -5.06e-3
#>  2 whisky a     -0.0356  0.00197 -0.00880  -0.00800    1.67e-3  4.14e-3  3.06e-5
#>  3 whisky a      0.0811 -0.00232 -0.00202   0.00307    7.56e-4  3.99e-3 -3.37e-3
#>  4 whisky a     -0.0694 -0.00396 -0.0115   -0.00718   -1.72e-3  4.63e-3 -1.78e-3
#>  5 whisky a     -0.0146  0.0455  -0.00662   0.00305   -2.45e-5 -1.69e-2  1.31e-3
#>  6 whisky a      0.121  -0.0208  -0.00168  -0.0115     2.42e-3  2.48e-3  2.55e-3
#>  7 whisky a     -0.0428 -0.0170  -0.00359  -0.000329  -3.13e-3 -1.78e-3 -4.73e-3
#>  8 whisky a      0.0343  0.00950 -0.000959 -0.00764    6.12e-3 -2.03e-3  5.70e-4
#>  9 whisky a      0.0114  0.00619  0.00207   0.00298    5.44e-3  2.31e-4 -3.57e-3
#> 10 whisky a     -0.0440  0.00410 -0.00285  -0.00367    3.75e-3 -7.95e-4  2.55e-4
#> # ℹ 30 more rows
#> # ℹ 13 more variables: PC8 <dbl>, PC9 <dbl>, PC10 <dbl>, PC11 <dbl>,
#> #   PC12 <dbl>, PC13 <dbl>, PC14 <dbl>, PC15 <dbl>, PC16 <dbl>, PC17 <dbl>,
#> #   PC18 <dbl>, PC19 <dbl>, PC20 <dbl>
bf %>% PCA %>% as_df(2) # or 2
#> # A tibble: 40 × 4
#>    type   fake      PC1      PC2
#>    <fct>  <fct>   <dbl>    <dbl>
#>  1 whisky a     -0.0520 -0.0226 
#>  2 whisky a     -0.0356  0.00197
#>  3 whisky a      0.0811 -0.00232
#>  4 whisky a     -0.0694 -0.00396
#>  5 whisky a     -0.0146  0.0455 
#>  6 whisky a      0.121  -0.0208 
#>  7 whisky a     -0.0428 -0.0170 
#>  8 whisky a      0.0343  0.00950
#>  9 whisky a      0.0114  0.00619
#> 10 whisky a     -0.0440  0.00410
#> # ℹ 30 more rows
bf %>% PCA %>% as_df(0.99) # or enough for 99%
#> # A tibble: 40 × 8
#>    type   fake      PC1      PC2       PC3       PC4        PC5       PC6
#>    <fct>  <fct>   <dbl>    <dbl>     <dbl>     <dbl>      <dbl>     <dbl>
#>  1 whisky a     -0.0520 -0.0226   0.00517  -0.00866   0.0131    -0.00109 
#>  2 whisky a     -0.0356  0.00197 -0.00880  -0.00800   0.00167    0.00414 
#>  3 whisky a      0.0811 -0.00232 -0.00202   0.00307   0.000756   0.00399 
#>  4 whisky a     -0.0694 -0.00396 -0.0115   -0.00718  -0.00172    0.00463 
#>  5 whisky a     -0.0146  0.0455  -0.00662   0.00305  -0.0000245 -0.0169  
#>  6 whisky a      0.121  -0.0208  -0.00168  -0.0115    0.00242    0.00248 
#>  7 whisky a     -0.0428 -0.0170  -0.00359  -0.000329 -0.00313   -0.00178 
#>  8 whisky a      0.0343  0.00950 -0.000959 -0.00764   0.00612   -0.00203 
#>  9 whisky a      0.0114  0.00619  0.00207   0.00298   0.00544    0.000231
#> 10 whisky a     -0.0440  0.00410 -0.00285  -0.00367   0.00375   -0.000795
#> # ℹ 30 more rows

# LDA object
bf %>% LDA(~fake) %>% as_df
#> removed these collinear columns:A1, B1, C1
#> # A tibble: 40 × 8
#>    actual predicted posterior type   fake     LD1    LD2     LD3
#>    <fct>  <fct>         <dbl> <fct>  <fct>  <dbl>  <dbl>   <dbl>
#>  1 a      a             1.000 whisky a     -4.19   1.84  -1.56  
#>  2 a      a             0.997 whisky a     -2.74   1.68  -0.0456
#>  3 a      a             0.878 whisky a     -2.26   0.171 -0.293 
#>  4 a      a             0.935 whisky a     -3.17  -0.527 -0.611 
#>  5 a      a             0.997 whisky a     -2.97   1.54   0.665 
#>  6 a      a             0.989 whisky a     -4.64  -0.937  0.385 
#>  7 a      d             0.620 whisky a     -0.109  2.26  -0.0356
#>  8 a      a             0.999 whisky a     -4.20   0.797  0.520 
#>  9 a      a             0.994 whisky a     -2.97   0.925 -0.640 
#> 10 a      b             0.826 whisky a     -1.52  -0.749  0.113 
#> # ℹ 30 more rows
# same options apply
```
