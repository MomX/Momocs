# Rearrange a matrix of (typically Fourier) coefficients

Momocs uses colnamed matrices to store (typically) Fourier coefficients
in [Coe](http://momx.github.io/Momocs/reference/Coe.md) objects
(typically [OutCoe](http://momx.github.io/Momocs/reference/OutCoe.md)).
They are arranged as rank-wise:
`A1, A2, ..., An, B1, ..., Bn, C1, ..., Cn, D1, ..., Dn`. From other
softwares they may arrive as `A1, B1, C1, D1, ..., An, Bn, Cn, Dn`, this
functions helps to go from one to the other format. In short, this
function rearranges column order. See examples.

## Usage

``` r
coeff_rearrange(x, by = c("name", "rank")[1])
```

## Arguments

- x:

  matrix (with colnames)

- by:

  character either "name" (`A1, A2, ..`) or "rank" (`A1, B1, ...`)

## Value

a Momocs object of same class

## Examples

``` r
m_name <- m_rank <- matrix(1:32, 2, 16)
# this one is ordered by name
colnames(m_name) <- paste0(rep(letters[1:4], each=4), 1:4)
# this one is ordered by rank
colnames(m_rank) <- paste0(letters[1:4], rep(1:4, each=4))

m_rank
#>      a1 b1 c1 d1 a2 b2 c2 d2 a3 b3 c3 d3 a4 b4 c4 d4
#> [1,]  1  3  5  7  9 11 13 15 17 19 21 23 25 27 29 31
#> [2,]  2  4  6  8 10 12 14 16 18 20 22 24 26 28 30 32
m_rank %>% coeff_rearrange(by="name")
#> Warning: `arrange_()` was deprecated in dplyr 0.7.0.
#> ℹ Please use `arrange()` instead.
#> ℹ See vignette('programming') for more help
#> ℹ The deprecated feature was likely used in the Momocs package.
#>   Please report the issue at <https://github.com/MomX/Momocs/issues>.
#>      a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 d1 d2 d3 d4
#> [1,]  1  9 17 25  3 11 19 27  5 13 21 29  7 15 23 31
#> [2,]  2 10 18 26  4 12 20 28  6 14 22 30  8 16 24 32
m_rank %>% coeff_rearrange(by="rank") #no change
#>      a1 b1 c1 d1 a2 b2 c2 d2 a3 b3 c3 d3 a4 b4 c4 d4
#> [1,]  1  3  5  7  9 11 13 15 17 19 21 23 25 27 29 31
#> [2,]  2  4  6  8 10 12 14 16 18 20 22 24 26 28 30 32

m_name
#>      a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 d1 d2 d3 d4
#> [1,]  1  3  5  7  9 11 13 15 17 19 21 23 25 27 29 31
#> [2,]  2  4  6  8 10 12 14 16 18 20 22 24 26 28 30 32
m_name %>% coeff_rearrange(by="name") # no change
#>      a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 d1 d2 d3 d4
#> [1,]  1  3  5  7  9 11 13 15 17 19 21 23 25 27 29 31
#> [2,]  2  4  6  8 10 12 14 16 18 20 22 24 26 28 30 32
m_name %>% coeff_rearrange(by="rank")
#>      a1 b1 c1 d1 a2 b2 c2 d2 a3 b3 c3 d3 a4 b4 c4 d4
#> [1,]  1  9 17 25  3 11 19 27  5 13 21 29  7 15 23 31
#> [2,]  2 10 18 26  4 12 20 28  6 14 22 30  8 16 24 32
```
