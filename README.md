
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sweets <a href="https://github.com/shum461/sweets"><img src="man/figures/logo.png" align="right" height="150" alt="sweets website" /></a>

<!-- badges: start -->
<!-- badges: end -->

**S**teve’s **W**orkflow for **E**numerating **E**legant **T**ext
listings and disposition**S**

## Installation

You can install the development version of sweets from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("shum461/sweets")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

fd <- get(data("fake_data"))
```

``` r

fd %>%
cnt(DELFN,STUDY,n_distinct_vars = USUBJID, prop = FALSE, pct = FALSE)
```

**Error: callr subprocess failed: ‘sweet_disposition’ is not an exported
object from ‘namespace:sweets’**

``` r

fd %>%
  sweet_disposition(subjid = USUBJID,
                      group_vars = STUDY)
```

# Count and Keeps

add deletion flags to ‘r cnt_n_keeps’  
if you wish to count but not remove samples and subjects

``` r

fd %>%
    sweet_disposition(subjid = USUBJID,
                      group_vars = STUDY, cnt_n_keeps = c(6,7))
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
