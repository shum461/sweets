# Get missing combinations

Quickly build vectors of missing combinations to help with further
sleuthing through missing data

## Usage

``` r
get_missing_combos(data, ...)
```

## Arguments

- data:

  a sweet_contents data.frame

- ...:

  provide a `name` variable from `sweet_contents` where n_missing is \>
  0

## Value

A vector of missing combinations and a message that shows selection e.g.
"VISIT" where "AMT" is missing

## Examples

``` r
if (FALSE) { # \dontrun{

    broken_pk %>%
    sweet_contents(missing_where = VISIT) %>%
    get_missing_combos("AMT")







} # }
```
