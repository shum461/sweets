
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sweets <a href="https://github.com/shum461/sweets"><img src="man/figures/logo.png" align="right" height="150" alt="sweets website" /></a>

<!-- badges: start -->
<!-- badges: end -->

**S**teve’s **W**orkflow for **E**numerating **E**legant **T**ext
listings and **S**AS dispositions

## Installation

You can install the development version of sweets from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("shum461/sweets")
#> Using github PAT from envvar GITHUB_PAT
#> Downloading GitHub repo shum461/sweets@HEAD
#> Skipping 1 packages ahead of CRAN: reporter
#> * checking for file ‘/tmp/RtmpTpsRBO/remotes16d554c5ccf/shum461-sweets-57c4dbc/DESCRIPTION’ ... OK
#> * preparing ‘sweets’:
#> * checking DESCRIPTION meta-information ... OK
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#> * looking to see if a ‘data/datalist’ file should be added
#>   NB: this package now depends on R (>= 3.5.0)
#>   WARNING: Added dependency on R >= 3.5.0 because serialized objects in
#>   serialize/load version 3 cannot be read in older versions of R.
#>   File(s) containing such objects:
#>     ‘sweets/data/fake_data.RData’
#> * building ‘sweets_0.0.0.9000.tar.gz’
#> Installing package into '/tmp/RtmpU7hdZu/temp_libpath1e6f23874f'
#> (as 'lib' is unspecified)
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
library(dmcognigen)
#> Loading dmcognigen Version 0.0.9000
library(sweets)

packageVersion('sweets')
#> [1] '0.0.0.9000'
# modified iris data
fd <- get(data("fake_data"))
```

``` r
fd %>%
cnt(DELFN,STUDY,n_distinct_vars = USUBJID, prop = FALSE, pct = FALSE)
#> # A tibble: 13 × 5
#>    DELFN STUDY      n_USUBJID     n n_cumulative
#>    <dbl> <chr>          <int> <int>        <int>
#>  1     0 setosa            12    46           46
#>  2     0 versicolor         9    38           84
#>  3     0 virginica         10    36          120
#>  4     4 setosa             3     8          128
#>  5     5 setosa            11    56          184
#>  6     5 versicolor         2     4          188
#>  7     5 virginica          1     2          190
#>  8     6 setosa             1     2          192
#>  9     6 versicolor        12    48          240
#> 10     6 virginica         11    40          280
#> 11     7 versicolor         5    14          294
#> 12     7 virginica          7    20          314
#> 13     8 virginica          3     6          320
```

**Error: callr subprocess failed: ‘sweet_disposition’ is not an exported
object from ‘namespace:sweets’**

``` r
# fd %>%
#   sweets::sweet_disposition(subjid = USUBJID,
#                       group_vars = STUDY)
```

# Count and Keeps

add deletion flags to ‘r cnt_n\_keeps’  
if you wish to count but not remove samples and subjects

``` r
# fd %>%
#     sweet_disposition(subjid = USUBJID,
#                       group_vars = STUDY, cnt_n_keeps = c(6,7)) 
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
