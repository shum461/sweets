#' Count observations and unique values by group
#'
#' @description
#' An extension of [dplyr::count()]. Counts are produced for unique
#' combinations of grouping variables; optionally includes proportions,
#' percentages, and cumulative counts. If any `n_distinct_vars` are provided,
#' counts of the unique values are produced for each.
#'
#' Adapted from `dmcognigen::cnt()` (Simulations Plus).
#'
#' @param .data data frame.
#' @param ... variables to group by.
#' @param n_distinct_vars variables to count the unique values of.
#' @param n_distinct_combined logical indicating whether to count the unique
#'   values of the combinations of `n_distinct_vars`.
#' @param n_cumulative logical indicating whether to include a cumulative sum
#'   variable named `"n_cumulative"`.
#' @param prop logical indicating whether to include a proportion variable named
#'   `"prop"`.
#' @param pct logical indicating whether to include a percentage variable named
#'   `"pct"`.
#'
#' @return A data frame with one row per unique combination of the grouping
#'   variables plus count columns.
#' @export
#'
#' @examples
#' # basic count
#' cnt(mtcars, cyl)
#'
#' # count unique values of am along with the number of distinct values of
#' # carb, cyl, and their combination.
#' cnt(mtcars, am, n_distinct_vars = c(carb, cyl))
cnt <- function(
    .data,
    ...,
    n_distinct_vars = NULL,
    n_distinct_combined = TRUE,
    n_cumulative = TRUE,
    prop = FALSE,
    pct = FALSE
) {

  # set visible bindings
  n <- NULL

  if (!is.data.frame(.data))      cli::cli_abort("{.arg .data} must be a data frame.")
  if (!is.logical(n_distinct_combined)) cli::cli_abort("{.arg n_distinct_combined} must be logical.")
  if (!is.logical(n_cumulative))  cli::cli_abort("{.arg n_cumulative} must be logical.")
  if (!is.logical(prop))          cli::cli_abort("{.arg prop} must be logical.")
  if (!is.logical(pct))           cli::cli_abort("{.arg pct} must be logical.")

  # if more than 1 n_distinct_vars are provided, create a combined variable
  # and count the unique number of combinations
  n_distinct_vars_names <- dplyr::select(.data, {{ n_distinct_vars }}) %>%
    names()

  if (length(n_distinct_vars_names) <= 1 || !isTRUE(n_distinct_combined)) {

    n_distinct_vars_all_name <- NULL

  } else {

    n_distinct_vars_all_name <- paste0(n_distinct_vars_names, collapse = "_")

    .data <- .data %>%
      tidyr::unite(
        col = !!n_distinct_vars_all_name,
        {{ n_distinct_vars }},
        sep = "_",
        remove = FALSE,
        na.rm = FALSE
      )

  }

  # variables that will be dropped based on user flags
  drop_vars_names <- c("n_cumulative", "prop", "pct")[!c(n_cumulative, prop, pct)]

  n_rows <- nrow(.data)

  .data %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(
      dplyr::across(
        .cols = c({{ n_distinct_vars }}, dplyr::any_of(n_distinct_vars_all_name)),
        .fns  = dplyr::n_distinct,
        .names = "n_{col}"
      ),
      n = dplyr::n(),
      prop = n / n_rows,
      pct  = 100 * n / n_rows,
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      n_cumulative = cumsum(n)
    ) %>%
    dplyr::select(
      -dplyr::any_of(drop_vars_names)
    )

}
