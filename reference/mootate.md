# Mutate and restore variable labels for sweet_read and other labelled objects

Works like dplyr::mutate(), but if the input object has class
"sweet_read", restores variable labels from the "var_labels" attribute
after mutation using purrr::walk. Otherwise, uses
sjlabelled::copy_labels() to restore labels from the original object.

## Usage

``` r
mootate(.data, ...)
```

## Arguments

- .data:

  A data frame (ideally of class "sweet_read")

- ...:

  Mutations (as in dplyr::mutate)

## Value

A data frame with labels restored.
