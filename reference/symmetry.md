# Calcuates symmetry indices on OutCoe objects

For [OutCoe](http://momx.github.io/Momocs/reference/OutCoe.md) objects
obtained with
[efourier](http://momx.github.io/Momocs/reference/efourier.md),
calculates several indices on the matrix of coefficients: `AD`, the sum
of absolute values of harmonic coefficients A and D; `BC` same thing for
B and C; `amp` the sum of the absolute value of all harmonic
coefficients and `sym` which is the ratio of `AD` over `amp`. See
references below for more details.

## Usage

``` r
symmetry(OutCoe)
```

## Arguments

- OutCoe:

  [efourier](http://momx.github.io/Momocs/reference/efourier.md) objects

## Value

a matrix with 4 colums described above.

## Note

What we call symmetry here is bilateral symmetry. By comparing
coefficients resulting from
[efourier](http://momx.github.io/Momocs/reference/efourier.md), with AD
responsible for amplitude of the Fourier functions, and BC for their
phase, it results in the plane and for fitted/reconstructed shapes that
symmetry. As long as your shapes are aligned along their bilateral
symmetry axis, you can use the approach coined by Iwata et al., and here
implemented in Momocs.

## References

Below: the first mention, and two applications.

- Iwata, H., Niikura, S., Matsuura, S., Takano, Y., & Ukai, Y. (1998).
  Evaluation of variation of root shape of Japanese radish (Raphanus
  sativus L.) based on image analysis using elliptic Fourier
  descriptors. Euphytica, 102, 143-149.

- Iwata, H., Nesumi, H., Ninomiya, S., Takano, Y., & Ukai, Y. (2002).
  The Evaluation of Genotype x Environment Interactions of Citrus Leaf
  Morphology Using Image Analysis and Elliptic Fourier Descriptors.
  Breeding Science, 52(2), 89-94. doi:10.1270/jsbbs.52.89

- Yoshioka, Y., Iwata, H., Ohsawa, R., & Ninomiya, S. (2004). Analysis
  of petal shape variation of Primula sieboldii by elliptic fourier
  descriptors and principal component analysis. Annals of Botany, 94(5),
  657-64. doi:10.1093/aob/mch190

## See also

[rm_asym](http://momx.github.io/Momocs/reference/rm_asym.md) and
[rm_sym](http://momx.github.io/Momocs/reference/rm_asym.md).

## Examples

``` r
bot.f <- efourier(bot, 12)
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details
res <- symmetry(bot.f)
hist(res[, 'sym'])
```
