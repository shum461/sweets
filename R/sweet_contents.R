





sweet_contents <- function(data,missing_where=NULL){



if(!rlang::quo_is_null(enquo(missing_where))){

  if( any(
      !data %>%
      dplyr::select({{missing_where}}) %>%
      names() %in% names(data))){

    cli::cli_abort(message = "{.arg {missing_where}} must be a variable in dataset")
  }
}



  missings <- data %>%
    dplyr::select(dplyr::where(~any(is.na(.)))) %>%
    names()

  all_missing  <- data %>%
    dplyr::select(dplyr::where(~all(is.na(.)))) %>%
    names()


  if(!length(missings)==0){

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


  missing_percents <- names(data) %>%
    purrr::map_dfr(~ dmcognigen::cnt(data,is.na(data[[.x]]),pct=TRUE) %>%
                     dplyr::mutate(name=.x)) %>%
    dplyr::filter(dplyr::if_any(dplyr::starts_with("is.na"))==TRUE) %>%
    dplyr::select(-dplyr::starts_with("is.na"),-n_cumulative) %>%
    dplyr::mutate(n_missing=tidyr::replace_na(n,0),
                  pct_missing=tidyr::replace_na(pct,0)) %>%
    dplyr::select(-c(n,pct))


 if(data %>%
      purrr::map_lgl(~is.null(attr(.x,"label"))) %>%
      all() ) {

      df <- tibble::tibble(name=names(data),label=NA_character_)%>%
        dplyr::left_join(missing_by_dvid,by="name") %>%
        dplyr::left_join(missing_percents,by="name")

      } else {

  df <- data %>%
    #dplyr::select(where(~any(is.na(.)))) %>%
    purrr::map(~attr(.x, "label")) %>%
    purrr::flatten() %>%
    tibble::enframe() %>%
    tidyr::unnest(cols=value) %>%
    dplyr::rename(label=value) %>%
    dplyr::left_join(missing_by_dvid,by="name") %>%
    dplyr::left_join(missing_percents,by="name")

}

 df %>%
   dplyr::select(-ends_with("missing_where_NULL")) %>%
   print(n= Inf)

}
