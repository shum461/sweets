# Make a disposition table

Count the number of subjects and samples impacted by a "deletion flag"
DELFN

## Usage

``` r
sweet_disposition(
  data,
  subjid,
  group_vars,
  cnt_n_keeps = NULL,
  init_desc = "Concentration Records Received"
)
```

## Arguments

- data:

  a data frame that must include deletion flags called DELFN.

- subjid:

  Subject Identifier such as USUBJID, or ID.

- group_vars:

  grouping variables useful in disposition tables such as STUDY or
  STUDYID. If none are provided a default group="1" will be set

- cnt_n_keeps:

  Deletion Flags DELFN that should be included in disposition table but
  do NOT remove subjects or rows.

- init_desc:

  Initial description of records received default is "Concentration
  Records Received"

## Value

A disposition table.

## Examples

``` r
broken_pk %>%
  dplyr::filter(EVID == 0) %>%
  sweet_disposition(subjid = USUBJID, group_vars = STUDYID)
#> # A tibble: 12 × 8
#> # Groups:   STUDYID [2]
#>    `Flag #` `Reason for Deletion` STUDYID `Samples Excluded` `Subjects Affected`
#>       <dbl> <chr>                 <chr>                <dbl>               <dbl>
#>  1        0 Concentration Record… CDISCP…                  0                   0
#>  2        0 Concentration Record… CDISCP…                  0                   0
#>  3        7 Placebo Group Not In… CDISCP…                689                  53
#>  4        7 Placebo Group Not In… CDISCP…                429                  33
#>  5        9 Missing Concentratio… CDISCP…                  8                   2
#>  6        9 Missing Concentratio… CDISCP…                  0                   0
#>  7       10 Missing Sample Date … CDISCP…                  8                   2
#>  8       10 Missing Sample Date … CDISCP…                  0                   0
#>  9       14 Missing Dose Date or… CDISCP…                  0                   0
#> 10       14 Missing Dose Date or… CDISCP…                 13                   1
#> 11       50 Analyst-Identified O… CDISCP…                  1                   1
#> 12       50 Analyst-Identified O… CDISCP…                  1                   1
#> # ℹ 3 more variables: `Subjects Excluded` <dbl>, `Remaining Samples` <dbl>,
#> #   `Remaining Subjects` <dbl>

```
