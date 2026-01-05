## Momocs

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![R-CMD-check](https://github.com/MomX/Momocs/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MomX/Momocs/actions/workflows/R-CMD-check.yaml)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/Momocs)](http://cran.r-project.org/package=Momocs)
[![Coverage
Status](https://img.shields.io/codecov/c/github/MomX/Momocs/master.svg)](https://codecov.io/github/MomX/Momocs?branch=master)
![CRAN downloads last
month](http://cranlogs.r-pkg.org/badges/Momocs)![CRAN downloads grand
total](http://cranlogs.r-pkg.org/badges/grand-total/Momocs)

### News

- I’m still looking for funding to develop MomX. If you have any idea,
  please email me `<bonhomme.vincent@gmail.com>`
- I’m available for consulting, training and collaboration, worldwide.
- Momocs is back on CRAN and no longer relies on the retired `rgeos`
  dependency
- The tutorial/introduction is back! Download it
  [there](https://github.com/MomX/Momocs/releases/download/v1.4.0/Momocs_intro.html)\*\*

### Installation

The last released version can be installed from
[CRAN](https://CRAN.R-project.org/package=Momocs) with:

``` r
install.packages("Momocs")
```

But I recommend using (and only support) the development version from
GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("MomX/Momocs")
```

### Example

This is a basic example of a complete analysis doing: inspection,
normalization of raw outlines, elliptical Fourier transforms,
dimmensionality reduction and classification, using a single line.

``` r
library(Momocs)
```

``` r
devtools::load_all()
#> ℹ Loading Momocs
#> Registered S3 method overwritten by 'vegan':
#>   method     from      
#>   rev.hclust dendextend
```

``` r
hearts %T>%                    # A toy dataset
  stack() %>%                  # Take a family picture of raw outlines
  fgProcrustes() %>%           # Full generalized Procrustes alignment
  coo_slide(ldk = 2) %T>%      # Redefine a robust 1st point between the cheeks
  stack() %>%                  # Another picture of aligned outlines
  efourier(6, norm=FALSE) %>%  # Elliptical Fourier Transforms
  PCA() %T>%                   # Principal Component Analysis
  plot_PCA(~aut) %>%           # A PC1:2 plot
  LDA(~aut) %>%                # Linear Discriminant Analysis
  plot_CV()                    # And the confusion matrix after leave one out cross validation
```

![](README-example-1.png)![](README-example-2.png)

``` R
#> Warning: The `<scale>` argument of `guides()` cannot be `FALSE`. Use "none" instead as
#> of ggplot2 3.3.4.
#> ℹ The deprecated feature was likely used in the Momocs package.
#>   Please report the issue at <https://github.com/MomX/Momocs/issues>.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
```

![](README-example-3.png)![](README-example-4.png)
