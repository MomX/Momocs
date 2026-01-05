# "Redo" a LDA on new data

Basically a wrapper around predict.lda from the package MASS. Uses a LDA
model to classify new data.

## Usage

``` r
reLDA(newdata, LDA)

# Default S3 method
reLDA(newdata, LDA)

# S3 method for class 'PCA'
reLDA(newdata, LDA)

# S3 method for class 'Coe'
reLDA(newdata, LDA)
```

## Arguments

- newdata:

  to use, a [PCA](http://momx.github.io/Momocs/reference/PCA.md) or any
  [Coe](http://momx.github.io/Momocs/reference/Coe.md) object

- LDA:

  a [LDA](http://momx.github.io/Momocs/reference/LDA.md) object

## Value

a list with components (from ?predict.lda ).

- class factor of classification

- posterior posterior probabilities for the classes

- x the scores of test cases

- res data.frame of the results

- CV.tab a confusion matrix of the results

- CV.correct proportion of the diagonal of CV.tab

- newdata the data used to calculate passed to predict.lda

## Note

Uses the same number of PC axis as the LDA object provided. You should
probably use [rePCA](http://momx.github.io/Momocs/reference/rePCA.md) in
conjunction with reLDA to get 'homologous' scores.

## Examples

``` r
# We select the first 10 individuals in bot,
# for whisky and beer bottles. It will be our referential.
bot1   <- slice(bot, c(1:10, 21:30))
# Same thing for the other 10 individuals.
# It will be our unknown dataset on which we want
# to calculate classes.
bot2   <- slice(bot, c(11:20, 31:40))

# We calculate efourier on these two datasets
bot1.f <- efourier(bot1, 8)
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details
bot2.f <- efourier(bot2, 8)
#> 'norm=TRUE' is used and this may be troublesome. See ?efourier #Details

# Here we obtain our LDA model: first, a PCA, then a LDA
bot1.p <- PCA(bot1.f)
bot1.l <- LDA(bot1.p, "type")
#> 6 PC retained

# we redo the same PCA since we worked with scores
bot2.p <- rePCA(bot1.p, bot2.f)

# we finally "predict" with the model obtained before
bot2.l <- reLDA(bot2.p, bot1.l)
bot2.l
#> $class
#>  [1] whisky whisky whisky whisky beer   whisky whisky beer   whisky whisky
#> [11] beer   beer   beer   whisky beer   whisky beer   beer   beer   beer  
#> Levels: beer whisky
#> 
#> $posterior
#>                        beer       whisky
#> jupiler        1.588583e-05 9.999841e-01
#> kingfisher     9.172746e-02 9.082725e-01
#> latrappe       2.276644e-02 9.772336e-01
#> lindemanskriek 5.840887e-03 9.941591e-01
#> nicechouffe    7.031244e-01 2.968756e-01
#> pecheresse     1.773492e-06 9.999982e-01
#> sierranevada   2.144720e-04 9.997855e-01
#> tanglefoot     9.372426e-01 6.275742e-02
#> tauro          1.183763e-05 9.999882e-01
#> westmalle      1.157018e-04 9.998843e-01
#> jb             9.975581e-01 2.441892e-03
#> johnniewalker  8.473188e-01 1.526812e-01
#> magallan       1.000000e+00 3.192177e-09
#> makersmark     1.093923e-01 8.906077e-01
#> oban           9.999880e-01 1.199656e-05
#> oldpotrero     2.425202e-02 9.757480e-01
#> redbreast      9.999820e-01 1.795276e-05
#> tamdhu         8.589367e-01 1.410633e-01
#> wildturkey     9.999905e-01 9.478960e-06
#> yoichi         9.744921e-01 2.550790e-02
#> 
#> $x
#>                       LD1
#> jupiler         2.9341875
#> kingfisher      0.6087998
#> latrappe        0.9982650
#> lindemanskriek  1.3640608
#> nicechouffe    -0.2289504
#> pecheresse      3.5163722
#> sierranevada    2.2430113
#> tanglefoot     -0.7179197
#> tauro           3.0122943
#> westmalle       2.4069164
#> jb             -1.5965434
#> johnniewalker  -0.4550552
#> magallan       -5.1945591
#> makersmark      0.5568188
#> oban           -3.0087530
#> oldpotrero      0.9810760
#> redbreast      -2.9017076
#> tamdhu         -0.4796867
#> wildturkey     -3.0712994
#> yoichi         -0.9673275
#> 
#> $newdata
#>                         PC1           PC2           PC3           PC4
#> jupiler         0.047558323 -0.0009556964 -1.132936e-02 -0.0038135827
#> kingfisher      0.031019804  0.0092893037 -5.639726e-03  0.0006984167
#> latrappe       -0.140467542  0.0368452619  8.204030e-03 -0.0074130929
#> lindemanskriek  0.028727335 -0.0093711139 -6.350860e-03  0.0040530566
#> nicechouffe     0.014137615 -0.0087352325 -1.464619e-03  0.0102437348
#> pecheresse      0.046019149 -0.0022071144 -1.263256e-02 -0.0019483303
#> sierranevada   -0.045574138  0.0101119946 -4.587309e-03 -0.0139742083
#> tanglefoot     -0.083848693  0.0019607973  1.265634e-02 -0.0086813906
#> tauro           0.047804962 -0.0010302173 -1.166643e-02 -0.0038315686
#> westmalle       0.043104213 -0.0006618641 -9.661507e-03  0.0013659528
#> jb              0.033826795 -0.0043832070  8.789680e-03 -0.0012973929
#> johnniewalker   0.027546559  0.0433572509 -5.503569e-07  0.0080407513
#> magallan        0.062757370  0.0344623824  1.994348e-02  0.0054305457
#> makersmark     -0.066073754 -0.0410683917 -2.500513e-02 -0.0028195062
#> oban            0.056001340 -0.0017415641  1.469954e-02  0.0024240694
#> oldpotrero     -0.039446859 -0.0560097481 -1.838963e-02  0.0143275711
#> redbreast      -0.070467008 -0.0453726482  1.443590e-03  0.0008230393
#> tamdhu          0.040245919  0.0099990120  8.849077e-03 -0.0054926858
#> wildturkey      0.009047941 -0.0118958979  1.825586e-02 -0.0002205672
#> yoichi         -0.041919333  0.0374066929  1.388610e-02  0.0020851880
#>                          PC5          PC6
#> jupiler         0.0005273680 -0.003411272
#> kingfisher     -0.0025505108 -0.000645225
#> latrappe        0.0014020218 -0.011164248
#> lindemanskriek -0.0005455535 -0.004342575
#> nicechouffe    -0.0031229185 -0.005062708
#> pecheresse      0.0030349867 -0.005659553
#> sierranevada   -0.0050825894 -0.003254912
#> tanglefoot     -0.0051466484 -0.003608419
#> tauro           0.0011495431 -0.003297832
#> westmalle       0.0015195495 -0.004960376
#> jb             -0.0020355558  0.003538980
#> johnniewalker  -0.0046821155 -0.006875580
#> magallan        0.0055312084  0.011826574
#> makersmark      0.0039534995  0.017991125
#> oban            0.0001554770  0.005786238
#> oldpotrero      0.0023407200  0.001001825
#> redbreast      -0.0038050474  0.012566253
#> tamdhu         -0.0011719141  0.000222802
#> wildturkey     -0.0020315441  0.003880876
#> yoichi          0.0105600234 -0.004531973
#> 
```
