# sweets

🧁 Tools for building clinical PK data disposition tables and deletion
listings.

## Installation

You can install the development version of sweets from
[GitHub](https://github.com/) with:

``` r
remotes::install_github("shum461/sweets")
```

# Functionality

## Create dispositions and listings

- Track counts of flagged or deleted records in a table using
  the[`sweet_disposition()`](https://shum461.github.io/sweets/reference/sweet_disposition.md)
  function.

- Deletion listings are tables of records removed for each deletion
  flag. Sweets builds off of the [reporter
  package](https://reporter.r-sassy.org/index.html) to include a
  workflow of initializing a listing report
  [`build_report()`](https://shum461.github.io/sweets/reference/build_report.md),
  populating the report
  [`add_listing_to_report()`](https://shum461.github.io/sweets/reference/add_listing_to_report.md)
  and writing the listings
  [`write_listings_report()`](https://shum461.github.io/sweets/reference/write_listings_report.md)

- See full details in the [dispositions-and-listings
  vignette](https://shum461.github.io/sweets/articles/dispositions-and-listings.html).

  🍬 Auto archive datasets before re-running code
  [`sweet_save()`](https://shum461.github.io/sweets/reference/sweet_save.md)  
  🍬 Hmisc::contents() but sweeter
  [`sweet_contents()`](https://shum461.github.io/sweets/reference/sweet_contents.md)  
  🍬 Track QC notes and findings
  [`airheads()`](https://shum461.github.io/sweets/reference/airheads.md)  
  🍬 See if modeling is underway
  [`check_models()`](https://shum461.github.io/sweets/reference/check_models.md)  
  🍬 Explore source datasets
  [`most_recent()`](https://shum461.github.io/sweets/reference/most_recent.md)
