# Check if modeling in underway

Sometimes it is useful for programmers to know if and when modeling has
occurred

## Usage

``` r
check_models(paths = c("../monolix/", "../nm/"))
```

## Arguments

- paths:

  to model files. Defaults to `paths=c("../monolix/","../nm/")` Provide
  the path(s) if you're not working in asmbdat directory

## Value

A data frame with software,output,user,min and max modification times

## Examples

``` r
if (FALSE) { # \dontrun{

# if working in asmbdat and looking just for monolix or nonmem, no arguments needed.
  check_models()

# otherwise provide the path(s)
check_models(path = "../misc/sponsor/sponsor-108/123456/d1pkpd/monolix/")
} # }
```
