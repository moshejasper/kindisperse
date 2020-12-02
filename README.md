
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
#> category:         FS 
#> dims:             100 
#> juvsigma      100 
#> breedsigma        50 
#> gravsigma         50 
#> ovisigma      25 
#> lifestage:        larva 
#> 
#> tab
#> # A tibble: 100 x 11
#>    id1   id2       x1     y1     x2     y2 ls1   ls2   distance category  dims
#>    <chr> <chr>  <dbl>  <dbl>  <dbl>  <dbl> <chr> <chr>    <dbl> <chr>    <dbl>
#>  1 1a    1b     41.7   77.3   73.4   98.0  larva larva     37.9 FS         100
#>  2 2a    2b     41.7  -28.5    5.51   8.31 larva larva     51.7 FS         100
#>  3 3a    3b      6.65  11.4   19.7   21.4  larva larva     16.5 FS         100
#>  4 4a    4b     33.2   59.4   40.4   -2.69 larva larva     62.5 FS         100
#>  5 5a    5b     44.8   29.9   26.9   58.7  larva larva     33.9 FS         100
#>  6 6a    6b     17.4  119.    55.0   92.9  larva larva     45.8 FS         100
#>  7 7a    7b     41.9   -1.76  38.5    9.27 larva larva     11.6 FS         100
#>  8 8a    8b    100.    42.7  103.    26.8  larva larva     16.1 FS         100
#>  9 9a    9b     28.0   83.6   51.0  108.   larva larva     33.8 FS         100
#> 10 10a   10b    95.9   70.8   87.7   37.9  larva larva     33.8 FS         100
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

    #> Loading required namespace: ggrepel

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
