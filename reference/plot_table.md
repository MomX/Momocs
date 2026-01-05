# Plots confusion matrix of sample sizes within \$fac

An utility that plots a confusion matrix of sample size (or a barplot)
for every object with a \$fac. Useful to visually how large are sample
sizes, how (un)balanced are designs, etc.

## Usage

``` r
plot_table(x, fac1, fac2 = fac1, rm0 = FALSE)
```

## Arguments

- x:

  any object with a \$fac slot (Coo, Coe, PCA, etc.)

- fac1:

  the name or id of the first factor

- fac2:

  the name of id of the second factor

- rm0:

  logical whether to print zeros

## Value

a ggplot2 object

## See also

Other plotting functions:
[`coo_arrows()`](http://momx.github.io/Momocs/reference/coo_arrows.md),
[`coo_draw()`](http://momx.github.io/Momocs/reference/coo_draw.md),
[`coo_listpanel()`](http://momx.github.io/Momocs/reference/coo_listpanel.md),
[`coo_lolli()`](http://momx.github.io/Momocs/reference/coo_lolli.md),
[`coo_plot()`](http://momx.github.io/Momocs/reference/coo_plot.md),
[`coo_ruban()`](http://momx.github.io/Momocs/reference/coo_ruban.md),
[`ldk_chull()`](http://momx.github.io/Momocs/reference/ldk_chull.md),
[`ldk_confell()`](http://momx.github.io/Momocs/reference/ldk_confell.md),
[`ldk_contour()`](http://momx.github.io/Momocs/reference/ldk_contour.md),
[`ldk_labels()`](http://momx.github.io/Momocs/reference/ldk_labels.md),
[`ldk_links()`](http://momx.github.io/Momocs/reference/ldk_links.md),
[`plot_devsegments()`](http://momx.github.io/Momocs/reference/plot_devsegments.md)

## Examples

``` r
plot_table(olea, "var")
#> Warning: `select_()` was deprecated in dplyr 0.7.0.
#> ℹ Please use `select()` instead.
#> ℹ The deprecated feature was likely used in the Momocs package.
#>   Please report the issue at <https://github.com/MomX/Momocs/issues>.

plot_table(olea, "domes", "var")

gg <- plot_table(olea, "domes", "var", rm0 = TRUE)
gg

library(ggplot2)
gg + coord_equal()

gg + scale_fill_gradient(low="green", high = "red")
#> Scale for fill is already present.
#> Adding another scale for fill, which will replace the existing scale.

gg + coord_flip()
```
