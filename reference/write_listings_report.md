# Write a listing report

Output path is set while initializing using
[`build_report()`](https://shum461.github.io/sweets/reference/build_report.md)
This function differs from
[`reporter::write_report()`](https://reporter.r-sassy.org/reference/write_report.html)
by including the N= counts at the end of each listing

## Usage

``` r
write_listings_report(deletion_report, ...)
```

## Arguments

- deletion_report:

  A deletion report built using
  [`build_report()`](https://shum461.github.io/sweets/reference/build_report.md)
  and
  [`add_listing_to_report()`](https://shum461.github.io/sweets/reference/add_listing_to_report.md)

- ...:

  Extra arguments to be passed to
  [`reporter::write_report()`](https://reporter.r-sassy.org/reference/write_report.html)

## Examples

``` r
if (FALSE) { # \dontrun{
write_listings_report(del_report)
} # }


```
