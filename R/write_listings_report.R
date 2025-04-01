#' Write a listing report
#'
#'@description
#' Output path is set while initializing using `build_report()`
#' This function differs from `reporter::write_report()`
#' by including the N= counts at the end of each listing
#' @export
#' @param deletion_report A deletion report built using `build_report()` and `add_listing_to_report()`
#' @param ...  Extra arguments to be passed to `reporter::write_report()`
#' @examples
#' \dontrun{
#' write_listings_report(del_report)
#' }
#'
#'
#'



write_listings_report = function(deletion_report,...){

Lines <- NULL
Delete <- NULL
  assertthat::assert_that(!purrr::is_null(deletion_report),
                          msg = "Build deletion report first")

  if(!class(deletion_report)[1]=="report_spec"){
    cli::cli_abort(c(
      "{.arg deletion_report} must be of a class {.cls 'report_spec/list'} from {.pkg reporter} pkg",
      "x" = "You tried to use a {.cls {class(deletion_report)}}."
    ))}

  # Set margins to standards
  deletion_report$margin_top=2
  deletion_report$margin_bottom=1
  deletion_report$margin_right=1
  deletion_report$margin_left=1

  # Output using reporter formats
  reporter::write_report(x=deletion_report,...)

  # Then Read it back in and fix it

  path <- paste0(deletion_report$file_path,".txt")


  if(!file.exists(path)){
    cli::cli_abort("{.file {path}} does not exist")
  }

  # The N= from each table are treated as new "objects"
  # in reporter and therefore \f page breaks are inappropriately added
  # Read in reporter output and get rid of extra spaces and breaks using readLines
  data <- brio::read_lines(path)

  listing_rows <- data.frame(Lines=seq_along(data),
                             Chars=nchar(data)
  )

  rows_to_delete <- listing_rows %>%
    dplyr::mutate(Delete=dplyr::case_when(
      Chars %in% c(0,1) & dplyr::lag(Chars) %in% c(0,3,4) ~ "Y",
      TRUE~ "N"
    )) %>%
    dplyr::filter(Delete=="Y") %>%
    dplyr::pull(Lines)

  output_data <- data[-rows_to_delete]

 readr::write_lines(x=output_data,file=path,append=FALSE)

}



#' Get listing report info and data
#'
#'@description
#' Get all of the data frames used when creating listing report
#'
#' @param deletion_report A deletion report built using `build_report()` and `add_listing_to_report()`
#' @export
#' @examples
#' \dontrun{
#' get_listing_df(del_report)
#' }
#'
#'


get_listing_info <- function(deletion_report,id_var=USUBJID){

# TODO Check Class


listing_table_objs <- seq(from = 1, to = length(deletion_report$content), by = 2)

listing_titles <- purrr::map_chr(listing_table_objs, ~deletion_report$content[[.x]]$object$titles[[1]][[1]])

listings_dataframes <- listing_table_objs %>%
  purrr::map(~deletion_report$content[[.x]]$object$data) %>%
  purrr::set_names(listing_titles)


listings_tibble <-
  tibble::tibble(Title=names(listings_dataframes),listing=listings_dataframes)

listing_data_names <- listing_table_objs %>%
purrr::map_dfr(~deletion_report$content[[.x]]$object$data) %>%
names()

id_var_quoted <- rlang::as_name(rlang::enquo(id_var))

if(id_var_quoted %in% listing_data_names) {

subjects <- purrr::map2_dfr(listing_table_objs, listing_titles, ~{
  data <- deletion_report$content[[.x]]$object$data
  if (nrow(data) > 0) {
    data %>% dplyr::mutate(Title = .y)
  } else {
    data
  }
}) %>% cnt(Title, n_distinct_vars = {{id_var}})


listings_tibble %>%
dplyr::left_join(subjects,by="Title")

} else{

  cli::cli_inform("{.val {id_var_quoted}} is not a variable in deletion report
                  and will be ignored")
  listings_tibble
}

}




