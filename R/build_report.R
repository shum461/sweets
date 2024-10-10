#' Initialize a listing report
#'
#' @param output_path Directory for listing output
#' @param file_name Name of listings file
#' @param ...  Extra arguments to be passed to reporter::options_fixed()
#' @examples
#' \dontrun{
#' del_report <- build_report(file_name = "deletion-listings")
#' }
#'
#'

build_report=function(output_path=".",file_name,...){

  path <- file.path(output_path,file_name)

  reporter::create_report(
    file_path = path,
    output_type = "TXT",
    orientation = "landscape",
    units = "inches",
    paper_size = "letter",
    missing = ".",
    font = "fixed",
    font_size = NULL
  ) %>%
    reporter::options_fixed(blank_margins = FALSE,
                  uchar = "",
                  editor = "word",
                  ...)

}
