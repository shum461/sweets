#' Make a pooled disposition table
#'
#' @param disp_table an object of class 'disposition_table'
#' @return A pooled disposition table.
#' @export
#' @examples
#' \dontrun{
#' pool_disposition(disp_table = my_disposition_table)
#' }


pool_disposition <- function(disp_table){

  `Remaining Subjects` <- NULL
   Remaining_subj <- NULL
  `Remaining Samples`  <- NULL
  `Flag #`  <- NULL
  `Reason for Deletion`  <- NULL
  `Subjects Affected`  <- NULL
   .  <- NULL
   `n` <- NULL

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
  grp_vars <- dplyr::group_vars(disp_table)

  pooled_disp <- disp_table %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(grp_vars, as.character)) %>%
    dplyr::group_by(`Flag #`, `Reason for Deletion`) %>%
    dplyr::summarise(
      dplyr::across(
        .cols = dplyr::where(is.numeric),
        .fns = sum
      ),
      .groups = "drop"
    ) %>%
    dplyr::bind_rows(dplyr::summarise(., dplyr::across(dplyr::where(is.numeric), sum),
                               dplyr::across(dplyr::where(is.character), ~ 'Grand Total'))) %>%
    dplyr::left_join(
      disp_table %>%
        dplyr::ungroup() %>%
        dplyr::distinct(`Reason for Deletion`, `Flag #`),
      by = c("Flag #", "Reason for Deletion")
    ) %>%
    dplyr::arrange(`Flag #`) %>%
    dplyr::select(-`Flag #`) %>%
    dplyr::mutate(dplyr::across(
      c(`Remaining Samples`, `Remaining Subjects`),
      ~ ifelse(`Reason for Deletion` == "Grand Total", lag(.x), .x)
    )) %>%
    dplyr::mutate(
      `Subjects Affected` = ifelse(
        `Reason for Deletion` == "Grand Total",
        attr(disp_table, "unique_aff_subjects"),
        `Subjects Affected`
      )
    )

  cli::cli_alert_info("Grand Total of {.var {'Subjects Affected'}} is unique subjects affected")

  pooled_disp

}
