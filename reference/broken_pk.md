# Example PK dataset for dispositions

Dataset based on `dmcognigen` example pk dataset with deletion flags
added and data intentionally manipulated for deletion examples

## Usage

``` r
broken_pk
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with
3556 rows and 107 columns.

## Examples

``` r
head(broken_pk)
#> # A tibble: 6 × 107
#>    ONUM   NUM STUDYID  USUBJID    ID   TSFD   TSPD NTSFD NTSPD  DVID DVIDC  EVID
#>   <int> <int> <chr>    <chr>   <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl> <chr> <dbl>
#> 1     1     1 CDISCPI… 01-701… 10101 0      0       0     0        0 Dose      1
#> 2     2     2 CDISCPI… 01-701… 10101 0.0833 0.0833  0.08  0.08     1 Xano…     0
#> 3     3     3 CDISCPI… 01-701… 10101 0.5    0.5     0.5   0.5      1 Xano…     0
#> 4     4     4 CDISCPI… 01-701… 10101 1      1       1     1        1 Xano…     0
#> 5     5     5 CDISCPI… 01-701… 10101 1.5    1.5     1.5   1.5      1 Xano…     0
#> 6     6     6 CDISCPI… 01-701… 10101 2      2       2     2        1 Xano…     0
#> # ℹ 95 more variables: MDV <dbl>, DV <dbl>, LOGDV <dbl>, BLQFN <dbl>,
#> #   LLOQ <dbl>, DOSE <dbl>, TRT <chr>, ROUTE <chr>, AMT <dbl>, FDDTTM <dttm>,
#> #   EDDTTM <dttm>, DNCP <dbl>, DTTM <dttm>, DAY <dbl>, RACEN <dbl>,
#> #   RACEC <chr>, SEXF <dbl>, SEXFC <chr>, HTCM <dbl>, WTKG <dbl>, AST <dbl>,
#> #   ASTULN <dbl>, SCR <dbl>, SCRULN <dbl>, TBIL <dbl>, TBILULN <dbl>,
#> #   ASTCAT <dbl>, BMI <dbl>, BSA <dbl>, IBW <dbl>, CRCL <dbl>, CRCLP <dbl>,
#> #   EGFR <dbl>, EGFRSCHW <dbl>, IBWCHILD <dbl>, LBM <dbl>, TBILCAT <dbl>, …


```
