
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sweets <a href="https://github.com/shum461/sweets"><img src="man/figures/logo.png" alt="sweets website" align="right" height="190"/></a>

<!-- badges: start -->
<!-- badges: end -->

🧁 Some sprinkles to make
[dmcognigen](https://github.com/simulations-plus/dmcognigen "dmcognigen GitHub")
a little sweeter

## Installation

You can install the development version of sweets from
[GitHub](https://github.com/) with:

``` r
remotes::install_github("shum461/sweets")
```

# Functionality

## Create dispositions and listings

- Track counts of flagged or deleted records in a table using
  the`sweet_disposition()` function.

- Deletion listings are tables of records removed for each deletion
  flag. Sweets builds off of the [reporter
  package](https://reporter.r-sassy.org/index.html) to include a
  workflow of initializing a listing report `build_report()`, populating
  the report `add_listing_to_report()` and writing the listings
  `write_listings_report()`

- See full details in the [dispositions-and-listings
  vignette](https://shum461.github.io/sweets/articles/dispositions-and-listings.html).
