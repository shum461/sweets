# delimp_biscuit

delimp_biscuit

## Usage

``` r
delimp_biscuit(delimp_paths, summary_tbl = TRUE)
```

## Arguments

- delimp_paths:

  a vector of one or more delimp directories

- summary_tbl:

  returns a data.frame of delimp files when TRUE. If FALSE \] the
  deletion dataset (i.e. data frame of all delimp files) is returned
  instead

## Value

a nested data frame showing deletion flags in each directory

## Examples

``` r
if(basename(getwd()) =="asmbdat" &&
dir.exists("../data/delimp") &&
length(list.files("../data/delimp")>=1 )) {

delimp_biscuit(delimp_paths = "../data/delimp", summary_tbl = TRUE)

}
```
