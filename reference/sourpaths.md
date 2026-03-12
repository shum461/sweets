# sour paths

a table of file paths ready to be pasted into an email

## Usage

``` r
sourpaths(full.names = TRUE, path = NULL, ...)
```

## Arguments

- full.names:

  Default is TRUE

- path:

  The directory path to pull from. Default is `'../data/'`

- ...:

  additional arguments passed to list.files()

## Value

a html DT::datatable() with a 'Copy' button

## Examples

``` r
if (FALSE) { # \dontrun{
sourpaths(path="data/",pattern="pk")
} # }
```
