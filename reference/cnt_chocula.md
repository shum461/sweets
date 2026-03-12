# Compare two counts

Compare two counts

## Usage

``` r
cnt_chocula(df1, df2, vars, ...)
```

## Arguments

- df1:

  first data frame to compare

- df2:

  second data frame to compare

- vars:

  variables to count

- ...:

  variables to count

## Value

A data frame with counts and differences.

## Examples

``` r
if (FALSE) { # \dontrun{

mtcars2 <- mtcars[mtcars$"cyl"> 4, ]

cnt_chocula(mtcars,mtcars2)

} # }
```
