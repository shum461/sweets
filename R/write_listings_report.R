#' Write a listing report
#'
#'@description
#' Output path is set while initializing using `build_report()`
#' This function differs from `reporter::write_report()`
#' by including the N= counts at the end of each listing
#'
#' @param deletion_report A deletion report built using `build_report()` and `add_listing_to_report()`
#' @param ...  Extra arguments to be passed to `reporter::write_report()`
#' @examples
#' write_listings_report(del_report)
#'
#'
#'



write_listings_report = function(deletion_report,...){


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
  data <- read_lines(path)

  listing_rows <- data.frame(Lines=seq_along(data),
                             Chars=nchar(data)
  )

  rows_to_delete <- listing_rows %>%
    mutate(Delete=case_when(
      Chars %in% c(0,1) & lag(Chars) %in% c(0,3,4) ~ "Y",
      TRUE~ "N"
    )) %>%
    filter(Delete=="Y") %>%
    pull(Lines)

  output_data <- data[-rows_to_delete]

  write_lines(x=output_data,file=path,append=FALSE)

}
