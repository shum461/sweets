#' Compare two counts
#'
#' @param df1 first data frame to compare
#' @param df2 second data frame to compare
#' @param ... variables to count
#' @param vars variables to count
#' @return A data frame with counts and differences.
#'
#' @examples
#' \dontrun{
#'
#' mtcars2 <- mtcars[mtcars$"cyl"> 4, ]
#'
#' cnt_chocula(mtcars,mtcars2)
#'
#' }
#' @export
#'
cnt_chocula <- function(df1,df2,vars,...){

  drop_vars <- c("n_1","n_2","n_diff","n_cumulative_1",
                 "n_cumulative_2","prop_1","prop_2","n_distinct_1","n_distinct_2")

  if (missing(df1) || missing(df2)) {
    cli::cli_abort("both {.arg df1} and {.arg df2} must be provided")
  }


  vars <- df1 %>%
    dplyr::select({{vars}}) %>%
    names()


  # Error if ... variables not in provided dataset(s)
  provided_to_df1 <- deparse(substitute(df1))
  not_in_df1 <- setdiff(vars,names(df1))

  if(!length(not_in_df1)==0){
    cli::cli_abort(message = "{.val {not_in_df1}} must be present in {.arg {provided_to_df1}}")
  }

  provided_to_df2 <- deparse(substitute(df2))
  not_in_df2 <- setdiff(vars,names(df2))

  if(!length(not_in_df2)==0){
    cli::cli_abort(message = "{.val {not_in_df2}} must be present in {.arg {provided_to_df2}}")
  }


  cli::cli_alert_info(text = "{.arg 1} = {.arg {provided_to_df1}}")
  cli::cli_alert_info(text = "{.arg 2} = {.arg {provided_to_df2}}")



  df1 %>%
    dmcognigen::cnt(dplyr::across(vars),...) %>%
    dplyr::full_join(
      df2 %>%
        dmcognigen::cnt(dplyr::across(vars),...),by=vars
    ) %>%
    dplyr::rename_with(~gsub(".x$","_1",.)) %>%
    dplyr::rename_with(~gsub(".y$","_2",.)) %>%
    dplyr::mutate(n_diff=n_1-n_2)%>%
    dplyr::select(-one_of(drop_vars),any_of(drop_vars))

}
