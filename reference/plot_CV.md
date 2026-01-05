# Plots a cross-validation table as an heatmap

Either with frequencies (or percentages) plus marginal sums, and values
as heatmaps. Used in Momocs for plotting cross-validation tables but may
be used for any table (likely with `freq=FALSE`).

## Usage

``` r
plot_CV(
  x,
  freq = FALSE,
  rm0 = FALSE,
  pc = FALSE,
  fill = TRUE,
  labels = TRUE,
  axis.size = 10,
  axis.x.angle = 45,
  cell.size = 2.5,
  signif = 2,
  ...
)

# Default S3 method
plot_CV(
  x,
  freq = FALSE,
  rm0 = FALSE,
  pc = FALSE,
  fill = TRUE,
  labels = TRUE,
  axis.size = 10,
  axis.x.angle = 45,
  cell.size = 2.5,
  signif = 2,
  ...
)

# S3 method for class 'LDA'
plot_CV(
  x,
  freq = TRUE,
  rm0 = TRUE,
  pc = TRUE,
  fill = TRUE,
  labels = TRUE,
  axis.size = 10,
  axis.x.angle = 45,
  cell.size = 2.5,
  signif = 2,
  ...
)
```

## Arguments

- x:

  a (cross-validation table) or an LDA object

- freq:

  logical whether to display frequencies (within an actual class) or
  counts

- rm0:

  logical whether to remove zeros

- pc:

  logical whether to multiply proportion by 100, ie display percentages

- fill:

  logical whether to fill cell according to count/freq

- labels:

  logical whether to add text labels on cells

- axis.size:

  numeric to adjust axis labels

- axis.x.angle:

  numeric to rotate x-axis labels

- cell.size:

  numeric to adjust text labels on cells

- signif:

  numeric to round frequencies using
  [signif](https://rdrr.io/r/base/Round.html)

- ...:

  useless here

## Value

a ggplot object

## See also

[LDA](http://momx.github.io/Momocs/reference/LDA.md),
[plot.LDA](http://momx.github.io/Momocs/reference/plot.LDA.md), and
(pretty much the same)
[plot_table](http://momx.github.io/Momocs/reference/plot_table.md).

## Examples

``` r
h <- hearts %>%
     fgProcrustes(0.01) %>% coo_slide(ldk=2) %T>% stack %>%
     efourier(6, norm=FALSE) %>% LDA(~aut)
#> iteration:  1    gain: 30322 
#> iteration:  2    gain: 1.2498 
#> iteration:  3    gain: 0.34194 
#> iteration:  4    gain: 0.0062954 


h %>% plot_CV()
#> Warning: The `<scale>` argument of `guides()` cannot be `FALSE`. Use "none" instead as
#> of ggplot2 3.3.4.
#> ℹ The deprecated feature was likely used in the Momocs package.
#>   Please report the issue at <https://github.com/MomX/Momocs/issues>.

h %>% plot_CV(freq=FALSE, rm0=FALSE, fill=FALSE)

# you can customize the returned gg with some ggplot2 functions
h %>% plot_CV(labels=FALSE, fill=TRUE, axis.size=5) + ggplot2::ggtitle("A confusion matrix")


# or build your own using the prepared data_frame:
df <- h %>% plot_CV() %$% data
#> Error in eval(substitute(expr), data, enclos = parent.frame()): invalid 'envir' argument of type 'object'
df
#> function (x, df1, df2, ncp, log = FALSE) 
#> {
#>     if (missing(ncp)) 
#>         .Call(C_df, x, df1, df2, log)
#>     else .Call(C_dnf, x, df1, df2, ncp, log)
#> }
#> <bytecode: 0x564c8b6d9100>
#> <environment: namespace:stats>

# you can even use it as a cross-table plotter
bot$fac %>% table %>% plot_CV()

```
