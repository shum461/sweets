# Count observations and unique values by group

An extension of
[`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html).
Counts are produced for unique combinations of grouping variables;
optionally includes proportions, percentages, and cumulative counts. If
any `n_distinct_vars` are provided, counts of the unique values are
produced for each.

Adapted from `dmcognigen::cnt()` (Simulations Plus).

## Usage

``` r
cnt(
  .data,
  ...,
  n_distinct_vars = NULL,
  n_distinct_combined = TRUE,
  n_cumulative = TRUE,
  prop = FALSE,
  pct = FALSE
)
```

## Arguments

- .data:

  data frame.

- ...:

  variables to group by.

- n_distinct_vars:

  variables to count the unique values of.

- n_distinct_combined:

  logical indicating whether to count the unique values of the
  combinations of `n_distinct_vars`.

- n_cumulative:

  logical indicating whether to include a cumulative sum variable named
  `"n_cumulative"`.

- prop:

  logical indicating whether to include a proportion variable named
  `"prop"`.

- pct:

  logical indicating whether to include a percentage variable named
  `"pct"`.

## Value

A data frame with one row per unique combination of the grouping
variables plus count columns.

## Examples

``` r
# basic count
cnt(mtcars, cyl)
#> # A tibble: 3 × 3
#>     cyl     n n_cumulative
#>   <dbl> <int>        <int>
#> 1     4    11           11
#> 2     6     7           18
#> 3     8    14           32

# count unique values of am along with the number of distinct values of
# carb, cyl, and their combination.
cnt(mtcars, am, n_distinct_vars = c(carb, cyl))
#> # A tibble: 2 × 6
#>      am n_carb n_cyl n_carb_cyl     n n_cumulative
#>   <dbl>  <int> <int>      <int> <int>        <int>
#> 1     0      4     3          7    19           19
#> 2     1      5     3          6    13           32
```
