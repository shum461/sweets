# make a listing table

`listing_table()` prepares a data subset to be added to
[`add_listing_to_report()`](https://shum461.github.io/sweets/reference/add_listing_to_report.md)
is used to build listing tables for each deletion data subset listing
tables can then be modified further using
[`reporter::define()`](https://reporter.r-sassy.org/reference/define.html)
or sent to
[`add_listing_to_report()`](https://shum461.github.io/sweets/reference/add_listing_to_report.md)
where listing tables are the second argument

## Usage

``` r
listing_table(listing, title, ...)
```

## Arguments

- listing:

  Data to be used in the listing. Subset to variables that best portray
  why the data were deleted. Selected variables should be source
  variables if possible

- title:

  Title to describe each unique listing in the report. Usually closely
  resembles DELFNC

- ...:

  additional arguments passed to
  [`reporter::create_table()`](https://reporter.r-sassy.org/reference/create_table.html)

## Value

A deletion report object that includes all added listings and their
titles of class "table_spec" & "list"

## Examples

``` r
if (FALSE) { # \dontrun{

 # Add tables to the report

del_report <- del_report %>%
 add_listing_to_report(
   listing_table(
     pk_delimp_mod  %>%
       filter(DELFN == 5, DVID == 2) %>%
       arrange(STUDYID, USUBJID, DTTM) %>%
       select(ROW,USUBJID, VISIT, PCTPT, PCTESTCD, PCSTAT),
     "Compound A (ng/mL), Sample Not Done / Not Recorded"
   ) %>%
     define(VISIT, label = "Cool New Visit Label", width = 0.15, id_var = TRUE, align = "left")
 )  %>%
 add_listing_to_report(
   listing_table(
     pk_delimp_mod %>%
       filter(DELFN == 5, DVID == 3) %>%
       arrange(STUDYID, USUBJID, DTTM) %>%
       select(ROW,USUBJID, VISIT, PCTPT, PCTESTCD, PCSTAT),
     "Compound B (ng/mL), Sample Not Done / Not Recorded"
   ))


} # }
```
