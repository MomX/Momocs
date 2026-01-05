# Silhouette plot

Only used, so far, after
[KMEDOIDS](http://momx.github.io/Momocs/reference/KMEDOIDS.md).

## Usage

``` r
plot_silhouette(x, palette = pal_qual)
```

## Arguments

- x:

  object returned by
  [KMEDOIDS](http://momx.github.io/Momocs/reference/KMEDOIDS.md)

- palette:

  one of [palettes](http://momx.github.io/Momocs/reference/palettes.md)

## Value

a `ggplot` plot

## Examples

``` r
olea %>% opoly(5) %>%
    KMEDOIDS(4) %>%
    plot_silhouette(pal_qual_solarized)
#> 'nb.pts' missing and set to 91
```
