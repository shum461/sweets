# Add each listing to the initialized report

`add_listing_to_report()` is used to build listings one deletion flag at
a time

## Usage

``` r
add_listing_to_report(deletion_report, listing, ...)
```

## Arguments

- deletion_report:

  Name of initialized deletion report made with
  [`build_report()`](https://shum461.github.io/sweets/reference/build_report.md)

- listing:

  Data to be used in the listing. Subset to variables that best portray
  why the data were deleted. Selected variables should be source
  variables if possible

- ...:

  additional arguments passed to
  [`reporter::create_table()`](https://reporter.r-sassy.org/reference/create_table.html)

## Value

A deletion report object that includes all added listings and their
titles

## Examples
