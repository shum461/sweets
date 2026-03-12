# Get listing report info and data

Get all of the data frames used when creating listing report

## Usage

``` r
get_listing_info(deletion_report, id_var = USUBJID)
```

## Arguments

- deletion_report:

  A deletion report built using
  [`build_report()`](https://shum461.github.io/sweets/reference/build_report.md)
  and
  [`add_listing_to_report()`](https://shum461.github.io/sweets/reference/add_listing_to_report.md)

- id_var:

  id variable such as USUBJID, ID, SUBJID. Default is USUBJID. Set to
  "none" to ignore

## Examples

``` r
if (FALSE) { # \dontrun{
get_listing_info(del_report)
} # }

```
