
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kindisperse

<!-- badges: start -->

<!-- badges: end -->

The goal of kindisperse is to simulate and estimate close-kin dispersal
kernels.

## Installation

You can install the released version of kindisperse from
[CRAN](https://CRAN.R-project.org) with: (in future. this doesn’t
currently exist)

``` r
install.packages("kindisperse")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("moshejasper/kindisperse")
```

## Introduction

Dispersal is a key evolutionary process that connects organisms in space
and time. Assessing the dispersal of organisms within an area is an
important component of estimating risks from invasive species, planning
pest management operations, and evaluating conservation strategies for
threatened species.

Assessing the dispersal of small, abundant and short-lived animals such
as insects as traditionally been more difficult than for animals that
can be easily tagged. Responding to this challenge, researchers have
developed various methods based around mark-release-recapture that mark
the organisms with dyes, paint, or chemical tags, before releasing the
individuals and in various ways measuring the number of recaptures.

Such methods suffer the limitations of requiring manipulation of the
same individuals in which dispersal is being assessed, are
labour-intensive when conducted across a large enough area to be
informative, and typically are not estimates of true intergenerational
dispersal (which is measured life-stage ot lifestage, e.g. from the egg
of the parent to the egg of its offspring). Such lifestage-to-lifestage
estimates are important as they are readily interpretable within
established intergenerational analytical frameworks such as Wright’s
neighbourhood size.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(kindisperse)
library(ggplot2)
## basic example code
simulate_kindist_composite()
#> KINDISPERSE SIMULATION of KIN PAIRS
#> -----------------------------------
#> simtype:      composite 
#> kerneltype:       Gaussian 
#> kinship:      FS 
#> simdims:      100 
#> juvsigma      100 
#> breedsigma        50 
#> gravsigma         50 
#> ovisigma      25 
#> lifestage:        larva 
#> 
#> tab
#> # A tibble: 100 x 8
#>    id1   id2   kinship distance     x1     y1     x2     y2
#>    <chr> <chr> <chr>      <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#>  1 1a    1b    FS         49.6   -8.66  69.1  -17.3  118.  
#>  2 2a    2b    FS         87.8   40.9   91.0   73.0    9.31
#>  3 3a    3b    FS         24.4   -5.43   3.36  11.0  -14.6 
#>  4 4a    4b    FS         95.6  104.    25.5   68.6  114.  
#>  5 5a    5b    FS         60.0   93.7  124.    34.6  114.  
#>  6 6a    6b    FS         29.1   35.1   44.9   33.8   74.0 
#>  7 7a    7b    FS         31.1   29.1   52.6   -1.18  45.4 
#>  8 8a    8b    FS         28.5  -27.4   40.4    1.11  40.6 
#>  9 9a    9b    FS         45.7   26.0   32.9   30.0   78.4 
#> 10 10a   10b   FS          5.69  64.6   -2.47  67.8   -7.20
#> # ... with 90 more rows
#> -----------------------------------
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-kinsim-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
