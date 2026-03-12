# Copy files from data directory to archive directory

Copy files from data directory to archive directory

## Usage

``` r
sweet_save(asmbdat_path = getwd(), archive_path = NULL, ...)
```

## Arguments

- asmbdat_path:

  path to projects asmbdat directory. Defaults to working directory

- archive_path:

  path to projects archive directory. Will create a new archive
  directory if one doesn't exists as well as dated folders inside
  archive directory. Dated archive directory defaults to System Date

- ...:

  arguments passed to fs::dir_info() i.e. regexp, glob

## Value

files moved to data archive directory.
