
#' delimp_biscuit
#'
#' @param delimp_paths a vector of one or more delimp directories
#'
#' @return a nested data frame showing deletion flags in each directory
#' @export
#'
#' @examples
#' \dontrun{
#' }

delimp_biscuit <- function(delimp_paths,summary_tbl=TRUE){

  if(missing(delimp_paths)){
    delimp_paths <- "../data/delimp"
  }


  found_delimp_files <- list.files(path = delimp_paths, full.names = TRUE)

  if(length(found_delimp_files)==0){
   cli::cli_alert(text = "no deletion dataset files found")
   return(invisible(NULL))
  }

found_delimp_files_df <- found_delimp_files %>%
    data.frame(files = .) %>%
    dplyr::mutate(base_names = sub(
      x = basename(files),
      pattern = ".xpt|.sas7bdat",
      replacement = ""
    ))

if(nrow(found_delimp_files_df)==0){
  cli::cli_alert(text = "no deletion dataset files found")
  return(invisible(NULL))
}




delimp_table <- found_delimp_files_df %>%
    dplyr::group_by(base_names) %>%
    dplyr::mutate(data =  purrr::map(files,  ~ rio::import(.x)) %>%
             purrr::set_names(base_names)) %>%
      dplyr::mutate(DELFN =  purrr::map_dbl(data,  ~ unique(pluck(.x, "DELFN"))))


if(summary_tbl) {
  return(delimp_table)
}



return(delimp_table %>%
dplyr::unnest(.cols=c(data)) %>%
  dplyr::select(-data)
)

  }
