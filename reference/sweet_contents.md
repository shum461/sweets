# Description of all variables

similar to Hmisc::contents() but is able to be filtered If data is
object of class `sweet_read` it will also show the path and last
modified time of source. and uses`missing_where` to show combinations of
missing variables e.g. WEIGHT missing where VISIT is "Screening, Week 1"

## Usage

``` r
sweet_contents(data, missing_where = NULL)
```

## Arguments

- data:

  data to observe

- missing_where:

  combinations of any variable of choice

## Value

A data frame with name, label (if it exists), n_missing, pct_missing if
there are any missings and class.

## Examples

``` r
if (FALSE) { # \dontrun{

  broken_pk %>%
  sweet_contents()

  broken_pk %>%
  sweet_contents(missing_where = DVID)%>%
  filter(stringr::str_detect(name,"^EX"))

  broken_pk %>%
  sweet_contents(missing_where = VISIT)
} # }
```
