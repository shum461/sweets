# most recent

most recent

## Usage

``` r
most_recent(path = NULL, pattern = NULL, newest_only = TRUE)
```

## Arguments

- path:

  filepath passed to `path` argument in fs::dir_ls(). Default is
  /dataorig

- pattern:

  one or more regex patters e.g. c("dm","lb") to be passed to `regexp`
  argument in fs::dir_ls()

- newest_only:

  return all paths or just the newest default is `TRUE`

## Value

data frame

## Examples

``` r
if (FALSE) { # \dontrun{
most_recent()
} # }
```
