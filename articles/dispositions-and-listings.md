# dispositions-and-listings

``` r
library(sweets)
#> Welcome 🍩 to 🍰 sweets 🍭
library(dplyr,warn.conflicts = FALSE)
```

During the data build records are flagged with a deletion flag variable
called DELFN and removed. Records that have been removed are stored in
the delimp directory. To do the disposition counts the removed records
are added back to the data build. For this vignette we are starting at
this step where deleted delimp records have already been added back to
the built analysis ready dataset.

## 1) Load a dataset

``` r
data(broken_pk, package = "sweets")
```

### Check the deletion flags.

``` r

broken_pk %>%
  cnt(DELFN,DELFNC,prop = FALSE)
#> # A tibble: 6 × 4
#>   DELFN DELFNC                                         n n_cumulative
#>   <dbl> <chr>                                      <int>        <int>
#> 1     0 Analysis Record                             2320         2320
#> 2     7 Placebo Group Not Included in the Analysis  1204         3524
#> 3     9 Missing Concentration Value                    8         3532
#> 4    10 Missing Sample Date and/or Time                8         3540
#> 5    14 Missing Dose Date or Time                     14         3554
#> 6    50 Analyst-Identified Outliers                    2         3556
```

### Check DVID

We want to track and count the removal of **concentration** records, not
**dose** records (i.e. DVID=0 & EVID=1). DELFN = 0 are records kept for
the analysis ready dataset build. Records with Non zero DELFN were
removed, stored in delimp and combined back for the disposition and
listings.

``` r

broken_pk %>%
  cnt(DVID,DVIDC,prop = FALSE)
#> # A tibble: 2 × 4
#>    DVID DVIDC                                n n_cumulative
#>   <dbl> <chr>                            <int>        <int>
#> 1     0 Dose                               254          254
#> 2     1 Xanomeline Concentration (ug/mL)  3302         3556
```

For the deletions that involve dose records, deletion flags are carried
from the dose record (DVID=0, EVID=1) onto the affected concentration
records (DVID=1) after the dose record of concern. Here we see all the
concentration records after the missing dose DTTM have flag 14.

``` r

broken_pk %>%
    group_by(USUBJID) %>%
    filter(any(DELFN==14)) %>%
    select(USUBJID,DTTM,DVID,TSFD,DELFN)
#> # A tibble: 14 × 5
#> # Groups:   USUBJID [1]
#>    USUBJID     DTTM                 DVID    TSFD DELFN
#>    <chr>       <dttm>              <dbl>   <dbl> <dbl>
#>  1 01-709-1326 NA                      0  0         14
#>  2 01-709-1326 2013-04-05 00:05:00     1  0.0833    14
#>  3 01-709-1326 2013-04-05 00:30:00     1  0.5       14
#>  4 01-709-1326 2013-04-05 01:00:00     1  1         14
#>  5 01-709-1326 2013-04-05 01:30:00     1  1.5       14
#>  6 01-709-1326 2013-04-05 02:00:00     1  2         14
#>  7 01-709-1326 2013-04-05 04:00:00     1  4         14
#>  8 01-709-1326 2013-04-05 06:00:00     1  6         14
#>  9 01-709-1326 2013-04-05 08:00:00     1  8         14
#> 10 01-709-1326 2013-04-05 12:00:00     1 12         14
#> 11 01-709-1326 2013-04-05 16:00:00     1 16         14
#> 12 01-709-1326 2013-04-06 00:00:00     1 24         14
#> 13 01-709-1326 2013-04-06 12:00:00     1 36         14
#> 14 01-709-1326 2013-04-07 00:00:00     1 48         14
```

## 2) Subset the data build to concentration i.e. sample records only

``` r

broken_pk_samples <- 
  broken_pk %>%
  filter(EVID==0)
```

Just concentration records are left

``` r
broken_pk_samples %>%
  cnt(DVID,DVIDC,prop = FALSE)
#> # A tibble: 1 × 4
#>    DVID DVIDC                                n n_cumulative
#>   <dbl> <chr>                            <int>        <int>
#> 1     1 Xanomeline Concentration (ug/mL)  3302         3302
```

## 3) Make a disposition table

Here we are splitting by STUDYID

``` r

disposition_table <- broken_pk_samples %>%
  sweet_disposition(subjid = USUBJID,group_vars = STUDYID) 

disposition_table %>%
  print(n = 15,width = Inf)
#> # A tibble: 12 × 8
#> # Groups:   STUDYID [2]
#>    `Flag #` `Reason for Deletion`                      STUDYID      `Samples Excluded` `Subjects Affected` `Subjects Excluded` `Remaining Samples` `Remaining Subjects`
#>       <dbl> <chr>                                      <chr>                     <dbl>               <dbl>               <dbl>               <dbl>                <dbl>
#>  1        0 Concentration Records Received             CDISCPILOT01                  0                   0                   0                2028                  156
#>  2        0 Concentration Records Received             CDISCPILOT02                  0                   0                   0                1274                   98
#>  3        7 Placebo Group Not Included in the Analysis CDISCPILOT01                689                  53                  53                1339                  103
#>  4        7 Placebo Group Not Included in the Analysis CDISCPILOT02                429                  33                  33                 845                   65
#>  5        9 Missing Concentration Value                CDISCPILOT01                  8                   2                   0                1331                  103
#>  6        9 Missing Concentration Value                CDISCPILOT02                  0                   0                   0                 845                   65
#>  7       10 Missing Sample Date and/or Time            CDISCPILOT01                  8                   2                   0                1323                  103
#>  8       10 Missing Sample Date and/or Time            CDISCPILOT02                  0                   0                   0                 845                   65
#>  9       14 Missing Dose Date or Time                  CDISCPILOT01                  0                   0                   0                1323                  103
#> 10       14 Missing Dose Date or Time                  CDISCPILOT02                 13                   1                   1                 832                   64
#> 11       50 Analyst-Identified Outliers                CDISCPILOT01                  1                   1                   0                1322                  103
#> 12       50 Analyst-Identified Outliers                CDISCPILOT02                  1                   1                   0                 831                   64
```

Someone changed their mind and now we want to keep DELFN 50
(Analyst-Identified Outliers) records in the dataset. Let’s set DELFN 50
as a ‘count and keep’ flag. Notice how the 2 samples that were
previously removed are now only ‘Affected’ and not ‘Excluded’

}

``` r

broken_pk_samples %>%
  sweet_disposition(subjid = USUBJID,
                    group_vars = STUDYID,
                    cnt_n_keeps = 50) %>%
  print(n = 15,width = Inf)
#> # A tibble: 12 × 8
#> # Groups:   STUDYID [2]
#>    `Flag #` `Reason for Deletion`                      STUDYID      `Samples Excluded` `Subjects Affected` `Subjects Excluded` `Remaining Samples` `Remaining Subjects`
#>       <dbl> <chr>                                      <chr>                     <dbl>               <dbl>               <dbl>               <dbl>                <dbl>
#>  1        0 Concentration Records Received             CDISCPILOT01                  0                   0                   0                2028                  156
#>  2        0 Concentration Records Received             CDISCPILOT02                  0                   0                   0                1274                   98
#>  3        7 Placebo Group Not Included in the Analysis CDISCPILOT01                689                  53                  53                1339                  103
#>  4        7 Placebo Group Not Included in the Analysis CDISCPILOT02                429                  33                  33                 845                   65
#>  5        9 Missing Concentration Value                CDISCPILOT01                  8                   2                   0                1331                  103
#>  6        9 Missing Concentration Value                CDISCPILOT02                  0                   0                   0                 845                   65
#>  7       10 Missing Sample Date and/or Time            CDISCPILOT01                  8                   2                   0                1323                  103
#>  8       10 Missing Sample Date and/or Time            CDISCPILOT02                  0                   0                   0                 845                   65
#>  9       14 Missing Dose Date or Time                  CDISCPILOT01                  0                   0                   0                1323                  103
#> 10       14 Missing Dose Date or Time                  CDISCPILOT02                 13                   1                   1                 832                   64
#> 11       50 Analyst-Identified Outliers                CDISCPILOT01                  0                   1                   0                1323                  103
#> 12       50 Analyst-Identified Outliers                CDISCPILOT02                  0                   1                   0                 832                   64
```

### Pool disposition

pooling a disposition with pool_disposition() will ignore any group
stratification set in your disposition table and add a Grand Total row.

``` r

pooled_disposition <- disposition_table %>%
  pool_disposition() 
#> ℹ Grand Total of `Subjects Affected` is unique subjects affected

pooled_disposition %>%
  print(n = 15, width = Inf)
#> # A tibble: 7 × 6
#>   `Reason for Deletion`                      `Samples Excluded` `Subjects Affected` `Subjects Excluded` `Remaining Samples` `Remaining Subjects`
#>   <chr>                                                   <dbl>               <dbl>               <dbl>               <dbl>                <dbl>
#> 1 Concentration Records Received                              0                   0                   0                3302                  254
#> 2 Placebo Group Not Included in the Analysis               1118                  86                  86                2184                  168
#> 3 Missing Concentration Value                                 8                   2                   0                2176                  168
#> 4 Missing Sample Date and/or Time                             8                   2                   0                2168                  168
#> 5 Missing Dose Date or Time                                  13                   1                   1                2155                  167
#> 6 Analyst-Identified Outliers                                 2                   2                   0                2153                  167
#> 7 Grand Total                                              1149                  93                  87                2153                  167
```

### Filtered disposition

For most projects it makes sense to include a “report” version of the
disposition table with only DELFN greater than 7

``` r

report_disposition <- disposition_table %>%
  filter(`Flag #`> 7) 

report_disposition %>%
  print(n = 15, width = Inf)
#> # A tibble: 8 × 8
#> # Groups:   STUDYID [2]
#>   `Flag #` `Reason for Deletion`           STUDYID      `Samples Excluded` `Subjects Affected` `Subjects Excluded` `Remaining Samples` `Remaining Subjects`
#>      <dbl> <chr>                           <chr>                     <dbl>               <dbl>               <dbl>               <dbl>                <dbl>
#> 1        9 Missing Concentration Value     CDISCPILOT01                  8                   2                   0                1331                  103
#> 2        9 Missing Concentration Value     CDISCPILOT02                  0                   0                   0                 845                   65
#> 3       10 Missing Sample Date and/or Time CDISCPILOT01                  8                   2                   0                1323                  103
#> 4       10 Missing Sample Date and/or Time CDISCPILOT02                  0                   0                   0                 845                   65
#> 5       14 Missing Dose Date or Time       CDISCPILOT01                  0                   0                   0                1323                  103
#> 6       14 Missing Dose Date or Time       CDISCPILOT02                 13                   1                   1                 832                   64
#> 7       50 Analyst-Identified Outliers     CDISCPILOT01                  1                   1                   0                1322                  103
#> 8       50 Analyst-Identified Outliers     CDISCPILOT02                  1                   1                   0                 831                   64
```

### Write disposition

``` r

disposition_tabs <- list("pooled"= pooled_disposition,
                         "report"= report_disposition,
                         "programmer"= disposition_table,
                         )

writexl::write_xlsx(disposition_tabs,"disposition.xlsx")
```

**🍭 It is best practice to refer back to the counts in your code and
check the results against your newly generated disposition table**

#### Troubleshooting

Use `subjects_in_flag` attribute to look for subjects by DELFN

``` r

attr(disposition_table,"subjects_in_flag") %>%
    filter(DELFN %in% c(50,10)) %>%
    tidyr::unnest(subjects_in_flag)
#> # A tibble: 4 × 2
#> # Groups:   DELFN [2]
#>   DELFN USUBJID    
#>   <dbl> <chr>      
#> 1    10 01-701-1341
#> 2    10 01-703-1403
#> 3    50 01-704-1325
#> 4    50 01-718-1254
```

## 4) Deletion Listings

#### Setup the report

Initialize the deletion listings report by calling
[`build_report()`](https://shum461.github.io/sweets/reference/build_report.md)

``` r

del_report <- build_report(file_name = "pk-deletion-listings")
```

**🍩 Listings are project specific. Always have a discussion about what
should be included.**

Check the flags and reasons in the disposition with samples excluded \>
0. We don’t need empty tables in our listings (e.g. CDISCPILOT02 Missing
Concentration Value). These remaining 7 rows indicate there should be 7
tables in our listings.

``` r
disposition_table %>%
  filter(`Samples Excluded`>0) %>%
  select(STUDYID,`Flag #`,`Reason for Deletion`,`Samples Excluded`)
#> # A tibble: 7 × 4
#> # Groups:   STUDYID [2]
#>   STUDYID      `Flag #` `Reason for Deletion`                      `Samples Excluded`
#>   <chr>           <dbl> <chr>                                                   <dbl>
#> 1 CDISCPILOT01        7 Placebo Group Not Included in the Analysis                689
#> 2 CDISCPILOT02        7 Placebo Group Not Included in the Analysis                429
#> 3 CDISCPILOT01        9 Missing Concentration Value                                 8
#> 4 CDISCPILOT01       10 Missing Sample Date and/or Time                             8
#> 5 CDISCPILOT02       14 Missing Dose Date or Time                                  13
#> 6 CDISCPILOT01       50 Analyst-Identified Outliers                                 1
#> 7 CDISCPILOT02       50 Analyst-Identified Outliers                                 1
```

### Add listing tables

Again we take our `broken_pk_samples`dataset consisting of the analysis
ready dataset plus the deleted records that have been removed during the
build and now added back. Listings are simply a consolidation of one or
more tables. These tables need to be customized to show variables that
make sense for each deletion reason. Perhaps we would want to show `TRT`
or `ACTARM` for `Placebo Group Not Included in the Analysis` or `DTTM`
for `Missing Sample Date and/or Time.`

**🍰 Try to use source variables over build variables in your
listings.**

If your dataset has labels, the label will appear as the variable name
on the table. To turn this off populate `...` of
[`add_listing_to_report()`](https://shum461.github.io/sweets/reference/add_listing_to_report.md)
using `use_attributes = 'none'`. See
[reporter::create_table()](https://reporter.r-sassy.org/reference/create_table.html)
for additional table specifications. If you need a quick way to add,
format, or modify an existing label add
[reporter::define()](https://reporter.r-sassy.org/reference/define.html)
customization to
[`listing_table()`](https://shum461.github.io/sweets/reference/listing_table.md)

``` r

# call the initialized deletion report 
# build listing tables and add them inside add_listing_to_report()
# Add tables to the report

del_report <- del_report %>%
  add_listing_to_report(
    listing_table(
    broken_pk_samples %>% 
    filter(STUDYID=='CDISCPILOT01', DELFN == 7) %>%
    arrange(STUDYID, USUBJID, DTTM) %>%
    select(STUDYID,USUBJID,TRT),
    title = "Study 01 Placebo Group Not Included in the Analysis")
  ) %>%
  add_listing_to_report(
    listing_table(
    broken_pk_samples %>% 
    filter(STUDYID=='CDISCPILOT02', DELFN == 7) %>%
    arrange(STUDYID, USUBJID, DTTM) %>%
    select(STUDYID,USUBJID,TRT,ACTARM),
    title = "Study 02 Placebo Group Not Included in the Analysis")
  )%>%
  add_listing_to_report(
    listing_table(
    broken_pk_samples %>% 
    filter(STUDYID=='CDISCPILOT01', DELFN == 9) %>%
    arrange(STUDYID, USUBJID, DTTM) %>%
    select(USUBJID,DTTM,VISIT,PCTPT,PCTEST,PCSTRESN),
    title = "Study 01 Missing Concentration Value")
  ) %>%
  add_listing_to_report(
    listing_table(
    broken_pk_samples %>% 
    filter(STUDYID=='CDISCPILOT01', DELFN == 10) %>%
    arrange(STUDYID, USUBJID, DTTM) %>%
    select(USUBJID,DTTM,VISIT,PCTPT,PCTEST,PCSTRESN),
    title = "Study 01 Missing Sample Date and/or Time")
  ) %>%
  add_listing_to_report(
    listing_table(
    broken_pk_samples %>% 
    filter(STUDYID=='CDISCPILOT02', DELFN == 14) %>%
    arrange(STUDYID, USUBJID, DTTM) %>%
    select(USUBJID,DTTM,VISIT,PCTPT,PCTEST,DVIDC,DV),
    title = "Study 02 Missing Sample Date and/or Time")
  )%>%
  add_listing_to_report(
    listing_table(
    broken_pk_samples %>% 
    filter(STUDYID=='CDISCPILOT01', DELFN == 50) %>%
    arrange(STUDYID, USUBJID, DTTM) %>%
    select(USUBJID,DTTM,VISIT,PCTPT,PCTEST,DVIDC,DV),
    title = "Study 01 Analyst-Identified Outliers")
  ) %>%
  add_listing_to_report(
    listing_table(
    broken_pk_samples %>% 
    filter(STUDYID=='CDISCPILOT02', DELFN == 50) %>%
    arrange(STUDYID, USUBJID, DTTM) %>%
    select(USUBJID,DTTM,VISIT,PCTPT,PCTEST,DVIDC,DV),
    title = "Study 02 Analyst-Identified Outliers") 
  )
#> ✔ Listing added to report.
#> ℹ There are now 1 listings total.
#> ℹ New listing attributes:
#> # A table specification:
#> - data: tibble 'listing' 689 rows 3 cols
#> - show_cols: all
#> - use_attributes: all
#> - title 1: 'Study 01 Placebo Group Not Included in the Analysis'
#> ✔ Listing added to report.
#> ℹ There are now 2 listings total.
#> ℹ New listing attributes:
#> # A table specification:
#> - data: tibble 'listing' 429 rows 4 cols
#> - show_cols: all
#> - use_attributes: all
#> - title 1: 'Study 02 Placebo Group Not Included in the Analysis'
#> ✔ Listing added to report.
#> ℹ There are now 3 listings total.
#> ℹ New listing attributes:
#> # A table specification:
#> - data: tibble 'listing' 8 rows 6 cols
#> - show_cols: all
#> - use_attributes: all
#> - title 1: 'Study 01 Missing Concentration Value'
#> ✔ Listing added to report.
#> ℹ There are now 4 listings total.
#> ℹ New listing attributes:
#> # A table specification:
#> - data: tibble 'listing' 8 rows 6 cols
#> - show_cols: all
#> - use_attributes: all
#> - title 1: 'Study 01 Missing Sample Date and/or Time'
#> ✔ Listing added to report.
#> ℹ There are now 5 listings total.
#> ℹ New listing attributes:
#> # A table specification:
#> - data: tibble 'listing' 13 rows 7 cols
#> - show_cols: all
#> - use_attributes: all
#> - title 1: 'Study 02 Missing Sample Date and/or Time'
#> ✔ Listing added to report.
#> ℹ There are now 6 listings total.
#> ℹ New listing attributes:
#> # A table specification:
#> - data: tibble 'listing' 1 rows 7 cols
#> - show_cols: all
#> - use_attributes: all
#> - title 1: 'Study 01 Analyst-Identified Outliers'
#> ✔ Listing added to report.
#> ℹ There are now 7 listings total.
#> ℹ New listing attributes:
#> # A table specification:
#> - data: tibble 'listing' 1 rows 7 cols
#> - show_cols: all
#> - use_attributes: all
#> - title 1: 'Study 02 Analyst-Identified Outliers'
```

[`get_listing_info()`](https://shum461.github.io/sweets/reference/get_listing_info.md)
allows you to see the listings added to the deletion report. This can be
useful when troubleshooting or comparing disposition counts to
listings.`id_var` argument adds subject counts. Default is `USUBJID`.
Change it to `ID` or `SUBJID` etc. based on your dataset or ignore it by
setting to `none`

``` r
get_listing_info(del_report)
#> # A tibble: 7 × 5
#>   Title                                               listing            n_USUBJID     n n_cumulative
#>   <chr>                                               <named list>           <int> <int>        <int>
#> 1 Study 01 Placebo Group Not Included in the Analysis <tibble [689 × 3]>        53   689          706
#> 2 Study 02 Placebo Group Not Included in the Analysis <tibble [429 × 4]>        33   429         1149
#> 3 Study 01 Missing Concentration Value                <tibble [8 × 6]>           2     8            9
#> 4 Study 01 Missing Sample Date and/or Time            <tibble [8 × 6]>           2     8           17
#> 5 Study 02 Missing Sample Date and/or Time            <tibble [13 × 7]>          1    13          720
#> 6 Study 01 Analyst-Identified Outliers                <tibble [1 × 7]>           1     1            1
#> 7 Study 02 Analyst-Identified Outliers                <tibble [1 × 7]>           1     1          707
```

``` r
get_listing_info(del_report,id_var = "none") %>%
  dplyr::slice(7) %>%
  tidyr::unnest(listing)
#> "none" is not a variable in deletion report and will be ignored
#> # A tibble: 1 × 8
#>   Title                                USUBJID     DTTM                VISIT    PCTPT            PCTEST     DVIDC                               DV
#>   <chr>                                <chr>       <dttm>              <chr>    <chr>            <chr>      <chr>                            <dbl>
#> 1 Study 02 Analyst-Identified Outliers 01-718-1254 2013-07-10 00:30:00 BASELINE 30 Min Post-dose XANOMELINE Xanomeline Concentration (ug/mL) 0.507
```

### Write Listings

file path by default will be the path specified during report
initialization with `build_report`. The `...` is for extra arguments to
be passed to
[reporter::write_report()](https://reporter.r-sassy.org/reference/write_report.html)

``` r

# Write Listings
write_listings_report(del_report)
```
