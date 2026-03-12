# Flexible file reader for SAS, XPT, and CSV formats

Reads a dataset from a path, supporting `.sas7bdat`, `.xpt`, and `.csv`
files. The original file path is retained as an attribute `"path"` on
the returned data. The file modification time (`"mtime"`) is also set as
an attribute.

## Usage

``` r
sweet_read(path, ..., record_input = TRUE)
```

## Arguments

- path:

  Character. Path to the file to read.

- ...:

  Additional arguments passed to
  [`data.table::fread`](https://rdrr.io/pkg/data.table/man/fread.html)
  (for .CSVs).

- record_input:

  Logical. Reserved for future use; currently ignored. Default is
  `TRUE`.

## Value

A data.frame or data.table with attributes `"path"` and `"mtime"` and
class `"sweet_read"`.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- read_flexible("mydata.csv")
df2 <- read_flexible("mydata.csv", record_input = FALSE)
} # }
```
