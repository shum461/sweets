
#' Make a pooled disposition table
#'
#' @param disp_table an object of class 'disposition_table'
#' @return A pooled disposition table.
#' @examples
#' pool_disposition(disp_table = my_disposition_table)





pool_disposition <- function(disp_table){

  if (is.null(attr(disp_table, "disposition_table"))) {
    user_provided_arg <- deparse(substitute(disp_table))

    cli::cli_abort(
      message = c(
        "{.arg {user_provided_arg}} is not a disposition table",
        "Your object should have been created using {.fun sweet_disposition}",
        "ensure the disposition_table attribute in {.arg {user_provided_arg}} is {.val TRUE}"
      )
    )
  }

  # Names of group vars
  grp_vars <- group_vars(disp_table)

  pooled_disp <- disp_table %>%
    ungroup() %>%
    mutate(across(grp_vars, as.character)) %>%
    group_by(`Flag #`, `Reason for Deletion`) %>%
    summarise(
      across(
        .cols = where(is.numeric),
        .fns = sum
      ),
      .groups = "drop"
    ) %>%
    bind_rows(summarise(., across(where(is.numeric), sum),
                        across(where(is.character), ~ 'Grand Total'))) %>%
    left_join(
      disp_table %>% ungroup() %>% distinct(`Reason for Deletion`, `Flag #`),
      by = c("Flag #", "Reason for Deletion")
    ) %>%
    arrange(`Flag #`) %>%
    select(-`Flag #`) %>%
    mutate(across(
      c(`Remaining Samples`, `Remaining Subjects`),
      ~ ifelse(`Reason for Deletion` == "Grand Total", lag(.x), .x)
    )) %>%
    mutate(
      `Subjects Affected` = ifelse(
        `Reason for Deletion` == "Grand Total",
        attr(disp_table, "unique_aff_subjects"),
        `Subjects Affected`
      )
    )

  cli::cli_alert_info("Grand Total of {.var {'Subjects Affected'}} is unique subjects affected")

  pooled_disp

}
