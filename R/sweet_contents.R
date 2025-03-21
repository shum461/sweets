#' Description of all variables
#'
#' @param data data to observe
#' @param missing_where combinations of any variable of choice
#' @return A data frame with name, label (if it exists), n_missing, pct_missing if there are any missings and class.
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#'
#'   broken_pk %>%
#'   sweet_contents()
#'
#'   broken_pk %>%
#'   sweet_contents(missing_where = DVID)
#'
#'   broken_pk %>%
#'   sweet_contents(missing_where = VISIT)
#' }
#' @export
#'



sweet_contents <- function(data,missing_where=NULL){


enquo <- NULL
value <- NULL
name <- NULL
n_cumulative <- NULL
n <- NULL
pct <- NULL
ends_with <- NULL


if(!rlang::quo_is_null(enquo(missing_where))){

  if( any(
      !data %>%
      dplyr::select({{missing_where}}) %>%
      names() %in% names(data))){

    cli::cli_abort(message = "{.arg {missing_where}} must be a variable in dataset")
  }
}



  missings <- data %>%
    dplyr::select(dplyr::where(~any(is.na(.))),-{{missing_where}}) %>%
    names()

  all_missing  <- data %>%
    dplyr::select(dplyr::where(~all(is.na(.)))) %>%
    names()


  if(!length(missings)==0){

  # Show where variables are missing
  missing_by_dvid <- data %>%
    dmcognigen::cnt(dplyr::across(dplyr::any_of(missings),is.na),{{missing_where}}) %>%
    tidyr::pivot_longer(dplyr::any_of(missings)) %>%
    dplyr::filter(value==TRUE) %>%
    dplyr::group_by(name) %>%
    dplyr::summarise("missing_where_{{missing_where}}":= paste(sort(unique({{missing_where}})),collapse=","))

  } else {


  missing_by_dvid <-
  tibble::tibble(name=names(data)) %>%
  dplyr::mutate("missing_where_{{missing_where}}":= "")

 }

 # Add percents and counts of missing
  missing_percents <- names(data) %>%
    purrr::map_dfr(~ dmcognigen::cnt(data,is.na(data[[.x]]),pct=TRUE) %>%
                     dplyr::mutate(name=.x)) %>%
    dplyr::filter(dplyr::if_any(dplyr::starts_with("is.na"))==TRUE) %>%
    dplyr::select(-dplyr::starts_with("is.na"),-n_cumulative) %>%
    dplyr::mutate(n_missing=tidyr::replace_na(n,0),
                  pct_missing=tidyr::replace_na(pct,0)) %>%
    dplyr::select(-c(n,pct))


  # Add class
  class <- tibble::tibble(
    name=names(data),
    class=purrr::map_chr(data,~class(.x)[1])
    )



 if(data %>%
      purrr::map_lgl(~is.null(attr(.x,"label"))) %>%
      all() ) {

      df <- tibble::tibble(name=names(data),label=NA_character_)%>%
        dplyr::left_join(missing_by_dvid,by="name") %>%
        dplyr::left_join(missing_percents,by="name")%>%
        dplyr::left_join(class,by="name")


      } else {

  df <- data %>%
    #dplyr::select(where(~any(is.na(.)))) %>%
    purrr::map(~attr(.x, "label")) %>%
    purrr::flatten() %>%
    tibble::enframe() %>%
    tidyr::unnest(cols=value) %>%
    dplyr::rename(label=value) %>%
    dplyr::left_join(missing_by_dvid,by="name") %>%
    dplyr::left_join(missing_percents,by="name")%>%
    dplyr::left_join(class,by="name")


}


  attr(df,"sweet_contents") <- TRUE

   df <- df %>%
   dplyr::select(-ends_with("missing_where_NULL")) %>%
   dplyr::mutate(dplyr::across(dplyr::starts_with("missing_where_"),~dplyr::na_if(.x,""))) %>%
   data.frame()


   return(df)


}


#' Get missing combinations
#'
#' @description
#' Quickly build vectors of missing combinations to help with further sleuthing
#' through missing data
#'
#' @param data a sweet_contents data.frame
#' @param ... provide a `name` variable from `sweet_contents` where n_missing is > 0
#' @return A vector of missing combinations and a message that shows selection e.g.
#' "VISIT" where "AMT" is missing
#'
#' @examples
#' \dontrun{
#'
#'     broken_pk %>%
#'     sweet_contents(missing_where = VISIT) %>%
#'     get_missing_combos("AMT")
#'
#'
#'
#'
#'
#'
#'
#' }
#' @export
#'


get_missing_combos <-  function(data,...){


    if(all(is.na(data$n_missing))){

      cli::cli_abort("No missings in dataset")
    }

   missing_where <- data %>%
    dplyr::select(dplyr::starts_with("missing_where_")) %>%
    names()


   if(length(missing_where)==0){
     cli::cli_abort(message="Populate the {.arg missing_where} argument in {.fun sweet_contents} first if you want to use
                    {.fun get_missing_combos}")
   }


   missing_vars_arg <- data %>%
   dplyr::filter(name %in% c(...)) %>%
   dplyr::pull(name)

    actual_missing_vars <- data %>%
    dplyr::filter(!is.na(n_missing)) %>%
    dplyr::pull(name)

    mismatch <- setdiff(missing_vars_arg,actual_missing_vars)


    if(!length(mismatch)==0){

      cli::cli_warn("{.val {mismatch}} did not have any missings to count")

    }


  where_name <- gsub("missing_where_",missing_where,replacement="")

  cli::cli_alert_info("{.val {where_name}} where {.val {missing_vars_arg}} is missing")

  data %>%
  dplyr::filter(name %in% missing_vars_arg) %>%
  tidyr::separate_rows(missing_where, sep =",") %>%
  dplyr::select(name,missing_where) %>%
  tidyr::drop_na() %>%
  dplyr::pull(starts_with("missing_where"))

}


